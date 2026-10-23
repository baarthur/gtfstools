#include <algorithm>
#include <climits>
#include <cmath>
#include <limits>
#include <utility>
#include <vector>

#include <cpp11.hpp>
using namespace cpp11;

// the kernels below run in parallel with OpenMP, when the package is compiled
// with it (e.g. not with Apple's clang), using as many threads as
// data.table::getDTthreads(). data.table is already a dependency and its
// thread count respects OMP_THREAD_LIMIT and setDTthreads(), so users and CRAN
// control both packages in the same way.
//
// inside parallel regions the R API is never used: inputs are read through
// raw pointers obtained beforehand (which also materializes ALTREP vectors on
// the main thread), outputs are allocated beforehand, and errors are raised
// after the region. each output element depends only on its own element,
// pattern or shape, without reductions across threads, so the results are the
// same regardless of the number of threads.
//
// called once per kernel call, on the main thread, before any parallel region

static int gtfs_threads() {
  const int n_threads = as_cpp<int>(package("data.table")["getDTthreads"]());
  return n_threads < 1 ? 1 : n_threads;
}

// great-circle distance, in meters, between two points, using the haversine
// formula. the sphere has the same radius used by {s2}, {sf}'s default engine
// for geographic coordinates, so the results match sf::st_distance() on
// lon/lat data. takes coordinates already in radians and the cosines of their
// latitudes, so that callers can calculate these once per point

static inline double haversine_radians(double lat_from, double lon_from,
                                       double cos_lat_from, double lat_to,
                                       double lon_to, double cos_lat_to) {
  const double earth_radius = 6371010.0; // = s2::s2_earth_radius_meters()

  const double sin_dlat = std::sin((lat_to - lat_from) / 2.0);
  const double sin_dlon = std::sin((lon_to - lon_from) / 2.0);
  double a = sin_dlat * sin_dlat +
    cos_lat_from * cos_lat_to * sin_dlon * sin_dlon;

  // rounding may push 'a' slightly outside [0, 1]

  if (a < 0.0) a = 0.0;
  if (a > 1.0) a = 1.0;

  return 2.0 * earth_radius * std::atan2(std::sqrt(a), std::sqrt(1.0 - a));
}

// the same, from coordinates in degrees

static double distance_haversine(double lat_from, double lon_from,
                                 double lat_to, double lon_to) {
  const double to_radians = 3.14159265358979323846 / 180.0;

  lat_from *= to_radians;
  lon_from *= to_radians;
  lat_to *= to_radians;
  lon_to *= to_radians;

  return haversine_radians(
    lat_from, lon_from, std::cos(lat_from),
    lat_to, lon_to, std::cos(lat_to)
  );
}

//' rcpp_distance_haversine
//'
//' Vectorize the above function. Returns NA where any coordinate is NA.
//'
//' @noRd
[[cpp11::register]]
doubles rcpp_distance_haversine(const doubles lat_from,
                                const doubles lon_from,
                                const doubles lat_to,
                                const doubles lon_to) {
  const R_xlen_t n = lat_from.size();
  if (lon_from.size() != n || lat_to.size() != n || lon_to.size() != n) {
    stop("All coordinate vectors must have the same length.");
  }

  writable::doubles distance(n);

  const double* lat_from_p = REAL(lat_from);
  const double* lon_from_p = REAL(lon_from);
  const double* lat_to_p = REAL(lat_to);
  const double* lon_to_p = REAL(lon_to);
  double* distance_p = REAL(distance);

  // small inputs aren't worth the threading overhead

  const int n_threads = n > 10000 ? gtfs_threads() : 1;
  (void) n_threads;

#ifdef _OPENMP
#pragma omp parallel for num_threads(n_threads) if(n_threads > 1)
#endif
  for (R_xlen_t i = 0; i < n; i++) {
    if (ISNAN(lat_from_p[i]) || ISNAN(lon_from_p[i]) ||
        ISNAN(lat_to_p[i]) || ISNAN(lon_to_p[i])) {
      distance_p[i] = NA_REAL;
    } else {
      distance_p[i] = distance_haversine(
        lat_from_p[i], lon_from_p[i], lat_to_p[i], lon_to_p[i]
      );
    }
  }

  return distance;
}

//' cpp_stop_distances
//'
//' Great-circle distance between the pairs of points given by 'from' and 'to'
//' (1-based indices into 'lat' and 'lon', in degrees), as calculated by
//' rcpp_distance_haversine() on the coordinates of each pair. The radians and
//' the cosine of the latitude of each point are calculated only once. Returns
//' NA where any coordinate of the pair is NA.
//'
//' @noRd
[[cpp11::register]]
doubles cpp_stop_distances(const doubles lat,
                           const doubles lon,
                           const integers from,
                           const integers to) {
  const R_xlen_t n_points = lat.size();
  const R_xlen_t n_pairs = from.size();
  if (lon.size() != n_points) {
    stop("Latitude and longitude vectors must have the same length.");
  }
  if (to.size() != n_pairs) {
    stop("'from' and 'to' must have the same length.");
  }

  const double* lat_p = REAL(lat);
  const double* lon_p = REAL(lon);
  const int* from_p = INTEGER(from);
  const int* to_p = INTEGER(to);

  for (R_xlen_t k = 0; k < n_pairs; k++) {
    if (from_p[k] == NA_INTEGER || from_p[k] < 1 || from_p[k] > n_points ||
        to_p[k] == NA_INTEGER || to_p[k] < 1 || to_p[k] > n_points) {
      stop("'from' and 'to' must be valid point indices.");
    }
  }

  // the same operations as in distance_haversine(), but once per point

  const double to_radians = 3.14159265358979323846 / 180.0;
  std::vector<double> lat_rad(n_points);
  std::vector<double> lon_rad(n_points);
  std::vector<double> cos_lat(n_points);
  for (R_xlen_t i = 0; i < n_points; i++) {
    lat_rad[i] = lat_p[i] * to_radians;
    lon_rad[i] = lon_p[i] * to_radians;
    cos_lat[i] = std::cos(lat_rad[i]);
  }

  writable::doubles distance(n_pairs);
  double* distance_p = REAL(distance);

  const int n_threads = n_pairs > 10000 ? gtfs_threads() : 1;
  (void) n_threads;

#ifdef _OPENMP
#pragma omp parallel for num_threads(n_threads) if(n_threads > 1)
#endif
  for (R_xlen_t k = 0; k < n_pairs; k++) {
    const R_xlen_t i = from_p[k] - 1;
    const R_xlen_t j = to_p[k] - 1;
    if (ISNAN(lat_p[i]) || ISNAN(lon_p[i]) ||
        ISNAN(lat_p[j]) || ISNAN(lon_p[j])) {
      distance_p[k] = NA_REAL;
    } else {
      distance_p[k] = haversine_radians(
        lat_rad[i], lon_rad[i], cos_lat[i], lat_rad[j], lon_rad[j], cos_lat[j]
      );
    }
  }

  return distance;
}

// wraps a longitude to the interval [reference - 180, reference + 180), so
// that shapes and stops crossing the antimeridian are handled correctly

static double unwrap_longitude(double lon, double reference) {
  double diff = lon - reference;
  diff -= 360.0 * std::floor((diff + 180.0) / 360.0);
  return reference + diff;
}

// locates the stops of one pattern along its shape (see
// cpp_locate_stops_on_shapes() below). doesn't use the R API, so it can run in
// parallel. may throw std::bad_alloc. returns the number of runs of the
// pruned dynamic program that were needed, or 0 if the full one was used

static int locate_stops_on_shape(const double* shape_lat,
                                 const double* shape_lon,
                                 const R_xlen_t n_points,
                                 const double* stop_lat,
                                 const double* stop_lon,
                                 const R_xlen_t n_stops,
                                 double* position,
                                 const bool prune) {
  if (n_stops == 0) return 0;

  const R_xlen_t n_segments = n_points - 1;
  const double earth_radius = 6371010.0;
  const double to_radians = 3.14159265358979323846 / 180.0;
  const double meters_per_degree = earth_radius * to_radians;

  // shape preparation: unwrapped longitudes, segment lengths, cumulative
  // distances and the east-west scale of each segment

  std::vector<double> lon(n_points);
  for (R_xlen_t k = 0; k < n_points; k++) {
    lon[k] = unwrap_longitude(shape_lon[k], shape_lon[0]);
  }

  // the radians and the cosine of the latitude of each point are calculated
  // only once, with the same operations as in distance_haversine()

  std::vector<double> lat_rad(n_points);
  std::vector<double> lon_rad(n_points);
  std::vector<double> cos_lat(n_points);
  for (R_xlen_t k = 0; k < n_points; k++) {
    lat_rad[k] = shape_lat[k] * to_radians;
    lon_rad[k] = shape_lon[k] * to_radians;
    cos_lat[k] = std::cos(lat_rad[k]);
  }

  std::vector<double> seg_length(n_segments);
  std::vector<double> cum_dist(n_segments);
  std::vector<double> x_scale(n_segments);
  double total = 0.0;

  for (R_xlen_t j = 0; j < n_segments; j++) {
    seg_length[j] = haversine_radians(
      lat_rad[j], lon_rad[j], cos_lat[j],
      lat_rad[j + 1], lon_rad[j + 1], cos_lat[j + 1]
    );
    cum_dist[j] = total;
    total += seg_length[j];

    const double mid_lat = (shape_lat[j] + shape_lat[j + 1]) / 2.0;
    x_scale[j] = meters_per_degree * std::cos(mid_lat * to_radians);
  }

  std::vector<double> stop_x(n_stops);
  for (R_xlen_t i = 0; i < n_stops; i++) {
    stop_x[i] = unwrap_longitude(stop_lon[i], shape_lon[0]);
  }

  // squared distance between stop i and the point at relative position 't' of
  // segment j. when 'project' is true, 't' is set to the position of the
  // stop's projection onto the segment (0 = start, 1 = end)

  auto squared_distance = [&](R_xlen_t i, R_xlen_t j, double& t, bool project) {
    const double bx = (lon[j + 1] - lon[j]) * x_scale[j];
    const double by = (shape_lat[j + 1] - shape_lat[j]) * meters_per_degree;
    const double px = (stop_x[i] - lon[j]) * x_scale[j];
    const double py = (stop_lat[i] - shape_lat[j]) * meters_per_degree;

    if (project) {
      const double length_sq = bx * bx + by * by;
      t = 0.0;
      if (length_sq > 0.0) {
        t = (px * bx + py * by) / length_sq;
        if (t < 0.0) t = 0.0;
        if (t > 1.0) t = 1.0;
      }
    }

    const double dx = px - t * bx;
    const double dy = py - t * by;
    return dx * dx + dy * dy;
  };

  auto project = [&](R_xlen_t i, R_xlen_t j, double& t) {
    return squared_distance(i, j, t, true);
  };

  // dynamic programming: prev[j] holds the minimum total cost of placing the
  // stops up to the previous one, with the previous one on segment j, at the
  // relative position prev_t[j]. the current stop may follow a stop placed on
  // an earlier segment, or a stop placed on the same segment. in the latter
  // case, if the stop is projected before the previous stop, it is placed at
  // the previous stop's position and the cost is the distance to that point,
  // so stops slightly out of order are handled in the same way on every
  // segment. ties keep the earliest segment. costs that differ by less than
  // 'tie_tolerance' (squared meters) are considered ties, otherwise rounding
  // errors would decide between overlapping parts of a shape (e.g. the two
  // directions of an out-and-back shape)

  const double tie_tolerance = 1e-6;
  std::vector<R_xlen_t> segment(n_stops);
  double t;

  // pruned dynamic programming. the full dynamic program below evaluates every
  // stop on every segment, but each stop is usually close to only a few of
  // them. let cf(i, j) be the squared distance between stop i and segment j,
  // d_i its minimum over the segments and D the sum of the d_i. each step of
  // the dynamic program adds a cost of at least d_i (the stay cost is never
  // smaller than cf), so the cost of a cell plus the d_k of the stops after it
  // is a lower bound of every cell derived from it, and the cost of a cell of
  // stop i is at least D - d_i + cf(i, j) when the d_k of the remaining stops
  // are added. the dynamic program is run only on the cells with
  // cf(i, j) <= d_i + band + margin, the others being treated as infinite. if
  // the resulting cost C satisfies C - D <= band, the result is the same as
  // that of the full dynamic program: by induction over the stops, every cell
  // whose bound is below D + band (minus the drift allowed by the tie rules)
  // has the same cost, position and back pointer in both, because the cells
  // left out are separated from them by more than 'tie_tolerance' and so
  // never win a comparison, nor change which of the remaining cells does. the
  // chosen cell of the last stop is one of these cells, and so are all the
  // cells it is traced back to. 'margin' covers the tie rules (up to about
  // 2 * tie_tolerance per stop) and rounding errors. otherwise the band is
  // widened and the dynamic program is run again. it gives up, and the full
  // dynamic program is used, when the band would keep more than a quarter of
  // the cells (so this never uses more memory than the full one), after 8
  // runs, or with coordinates for which the bounds below don't hold
  //
  // the bound of cf(i, j) is calculated for chunks of 'chunk_size'
  // consecutive segments, from the bounding box of their points and the
  // smallest east-west scale of their segments (cos() decreases with the
  // absolute latitude only up to 90 degrees)

  auto locate_pruned = [&]() -> int {
    for (R_xlen_t k = 0; k < n_points; k++) {
      if (!std::isfinite(lon[k]) || !(std::fabs(shape_lat[k]) <= 90.0)) {
        return 0;
      }
    }
    for (R_xlen_t i = 0; i < n_stops; i++) {
      if (!std::isfinite(stop_x[i]) || !(std::fabs(stop_lat[i]) <= 90.0)) {
        return 0;
      }
    }

    const double inf = std::numeric_limits<double>::infinity();
    const R_xlen_t chunk_size = 8;
    const R_xlen_t n_chunks = (n_segments + chunk_size - 1) / chunk_size;
    auto chunk_end = [&](R_xlen_t c) {
      return std::min(c * chunk_size + chunk_size, n_segments);
    };

    // bounding box and smallest east-west scale of each chunk

    std::vector<double> box_lon_min(n_chunks);
    std::vector<double> box_lon_max(n_chunks);
    std::vector<double> box_lat_min(n_chunks);
    std::vector<double> box_lat_max(n_chunks);
    std::vector<double> box_x_scale(n_chunks);

    for (R_xlen_t c = 0; c < n_chunks; c++) {
      const R_xlen_t first = c * chunk_size;
      box_lon_min[c] = box_lon_max[c] = lon[first];
      box_lat_min[c] = box_lat_max[c] = shape_lat[first];
      box_x_scale[c] = x_scale[first];
      for (R_xlen_t j = first; j < chunk_end(c); j++) {
        box_lon_min[c] = std::min(box_lon_min[c], lon[j + 1]);
        box_lon_max[c] = std::max(box_lon_max[c], lon[j + 1]);
        box_lat_min[c] = std::min(box_lat_min[c], shape_lat[j + 1]);
        box_lat_max[c] = std::max(box_lat_max[c], shape_lat[j + 1]);
        box_x_scale[c] = std::min(box_x_scale[c], x_scale[j]);
      }
    }

    // lower bound of the squared distance between each stop and the segments
    // of each chunk

    std::vector<double> bound((size_t) n_stops * (size_t) n_chunks);

    for (R_xlen_t i = 0; i < n_stops; i++) {
      for (R_xlen_t c = 0; c < n_chunks; c++) {
        const double dlon = std::max(
          0.0,
          std::max(box_lon_min[c] - stop_x[i], stop_x[i] - box_lon_max[c])
        );
        const double dlat = std::max(
          0.0,
          std::max(box_lat_min[c] - stop_lat[i], stop_lat[i] - box_lat_max[c])
        );
        const double dx = dlon * box_x_scale[c];
        const double dy = dlat * meters_per_degree;
        bound[(size_t) i * (size_t) n_chunks + (size_t) c] = dx * dx + dy * dy;
      }
    }

    // d_i: the chunk with the smallest bound is searched first, then only the
    // chunks whose bound doesn't exceed the minimum found so far. if that
    // already means evaluating more than 'max_cells' cells (e.g. stops near
    // the centre of a loop), pruning wouldn't pay

    const double max_cells = 0.25 * (double) n_stops * (double) n_segments;
    std::vector<double> nearest(n_stops);
    double nearest_sum = 0.0;
    double n_searched = 0.0;

    for (R_xlen_t i = 0; i < n_stops; i++) {
      const double* stop_bound = &bound[(size_t) i * (size_t) n_chunks];
      double best = inf;
      auto search = [&](R_xlen_t c) {
        for (R_xlen_t j = c * chunk_size; j < chunk_end(c); j++) {
          double t_free;
          best = std::min(best, project(i, j, t_free));
        }
        n_searched += chunk_end(c) - c * chunk_size;
      };

      R_xlen_t closest = 0;
      for (R_xlen_t c = 1; c < n_chunks; c++) {
        if (stop_bound[c] < stop_bound[closest]) closest = c;
      }
      search(closest);
      for (R_xlen_t c = 0; c < n_chunks; c++) {
        if (c != closest && stop_bound[c] <= best) search(c);
      }

      nearest[i] = best;
      nearest_sum += best;
      if (n_searched > max_cells) return 0;
    }

    // the cells of each stop are stored contiguously, in the order of their
    // segments: 'cell_segment' holds their segments and 'cell_back' the
    // segments of the cells of the previous stop they follow. the costs and
    // positions are only kept for the previous and the current stops

    std::vector<size_t> row_start(n_stops + 1);
    std::vector<int> cell_segment;
    std::vector<int> cell_back;
    std::vector<double> prev_cost;
    std::vector<double> prev_t;
    std::vector<double> cur_cost;
    std::vector<double> cur_t;
    double band = 1.0;

    for (int run = 1; run <= 8; run++) {
      const double margin = 1.0 + 1e-9 * (nearest_sum + band) +
        4.0 * (double) n_stops * tie_tolerance;

      // the cells are counted before any allocation

      size_t n_cells = 0;
      size_t max_row = 0;
      for (R_xlen_t i = 0; i < n_stops; i++) {
        row_start[i] = n_cells;
        const double limit = nearest[i] + band + margin;
        const double* stop_bound = &bound[(size_t) i * (size_t) n_chunks];
        for (R_xlen_t c = 0; c < n_chunks; c++) {
          if (stop_bound[c] <= limit) n_cells += chunk_end(c) - c * chunk_size;
        }
        max_row = std::max(max_row, n_cells - row_start[i]);
      }
      row_start[n_stops] = n_cells;

      if ((double) n_cells > max_cells) return 0;

      cell_segment.resize(n_cells);
      cell_back.assign(n_cells, -1);
      for (R_xlen_t i = 0; i < n_stops; i++) {
        size_t k = row_start[i];
        const double limit = nearest[i] + band + margin;
        const double* stop_bound = &bound[(size_t) i * (size_t) n_chunks];
        for (R_xlen_t c = 0; c < n_chunks; c++) {
          if (stop_bound[c] > limit) continue;
          for (R_xlen_t j = c * chunk_size; j < chunk_end(c); j++) {
            cell_segment[k++] = (int) j;
          }
        }
      }

      prev_cost.resize(max_row);
      prev_t.resize(max_row);
      cur_cost.resize(max_row);
      cur_t.resize(max_row);

      for (size_t k = row_start[0]; k < row_start[1]; k++) {
        prev_cost[k] = project(0, cell_segment[k], prev_t[k]);
      }

      // the same recurrence as in the full dynamic program below, where the
      // segments of the previous stop without a cell are infinite: they can't
      // be stayed on, and they never become 'earlier_arg' once a finite cell
      // has been seen

      for (R_xlen_t i = 1; i < n_stops; i++) {
        const size_t prev_first = row_start[i - 1];
        const size_t prev_end = row_start[i];
        size_t p = prev_first;
        double earlier_min = 0.0;
        int earlier_arg = -1;

        for (size_t k = row_start[i]; k < row_start[i + 1]; k++) {
          const int j = cell_segment[k];

          while (p < prev_end && cell_segment[p] < j) {
            const double cost = prev_cost[p - prev_first];
            if (earlier_arg < 0 || cost < earlier_min - tie_tolerance) {
              earlier_min = cost;
              earlier_arg = cell_segment[p];
            }
            p++;
          }

          double t_free;
          const double cost_free = project(i, j, t_free);

          double total = inf;
          double total_t = t_free;
          int best_arg = -1;

          if (p < prev_end && cell_segment[p] == j) {
            const double stay_from = prev_t[p - prev_first];
            double t_stay = t_free;
            double cost_stay = cost_free;
            if (t_free < stay_from) {
              t_stay = stay_from;
              cost_stay = squared_distance(i, j, t_stay, false);
            }
            total = prev_cost[p - prev_first] + cost_stay;
            total_t = t_stay;
            best_arg = j;
          }

          if (earlier_arg >= 0 &&
              earlier_min + cost_free <= total + tie_tolerance) {
            total = earlier_min + cost_free;
            total_t = t_free;
            best_arg = earlier_arg;
          }

          cur_cost[k - row_start[i]] = total;
          cur_t[k - row_start[i]] = total_t;
          cell_back[k] = best_arg;
        }

        std::swap(prev_cost, cur_cost);
        std::swap(prev_t, cur_t);
      }

      const size_t last_first = row_start[n_stops - 1];
      const size_t last_end = row_start[n_stops];
      size_t best = last_first;
      double cost = inf;
      if (last_end > last_first) {
        for (size_t k = last_first + 1; k < last_end; k++) {
          if (prev_cost[k - last_first] <
              prev_cost[best - last_first] - tie_tolerance) {
            best = k;
          }
        }
        cost = prev_cost[best - last_first];
      }

      if (cost < inf && cost - nearest_sum <= band) {
        segment[n_stops - 1] = cell_segment[best];
        size_t k = best;
        for (R_xlen_t i = n_stops - 1; i > 0; i--) {
          const int s = cell_back[k];
          segment[i - 1] = s;
          k = std::lower_bound(
            cell_segment.begin() + row_start[i - 1],
            cell_segment.begin() + row_start[i],
            s
          ) - cell_segment.begin();
        }
        return run;
      }

      // without any path through the cells (e.g. stops in the opposite order
      // of the shape), the band is widened faster

      band = cost < inf ? std::max(cost - nearest_sum, 16.0 * band)
                        : 256.0 * band;
    }

    return 0;
  };

  const int dp_runs = prune ? locate_pruned() : 0;

  // the full dynamic program, when pruning is off or gave up

  if (dp_runs == 0) {
    std::vector<double> prev(n_segments);
    std::vector<double> cur(n_segments);
    std::vector<double> prev_t(n_segments);
    std::vector<double> cur_t(n_segments);
    std::vector<int> back((size_t) n_stops * (size_t) n_segments);

    for (R_xlen_t j = 0; j < n_segments; j++) {
      prev[j] = project(0, j, t);
      prev_t[j] = t;
    }

    for (R_xlen_t i = 1; i < n_stops; i++) {
      // minimum cost (and its segment) among the segments before segment j
      double earlier_min = 0.0;
      int earlier_arg = -1;

      for (R_xlen_t j = 0; j < n_segments; j++) {
        double t_free;
        const double cost_free = project(i, j, t_free);

        // staying on the same segment as the previous stop: the stop can't be
        // placed before it
        double t_stay = t_free;
        double cost_stay = cost_free;
        if (t_free < prev_t[j]) {
          t_stay = prev_t[j];
          cost_stay = squared_distance(i, j, t_stay, false);
        }

        double total = prev[j] + cost_stay;
        double total_t = t_stay;
        int best_arg = (int) j;

        if (earlier_arg >= 0 &&
            earlier_min + cost_free <= total + tie_tolerance) {
          total = earlier_min + cost_free;
          total_t = t_free;
          best_arg = earlier_arg;
        }

        cur[j] = total;
        cur_t[j] = total_t;
        back[(size_t) i * (size_t) n_segments + (size_t) j] = best_arg;

        if (earlier_arg < 0 || prev[j] < earlier_min - tie_tolerance) {
          earlier_min = prev[j];
          earlier_arg = (int) j;
        }
      }

      std::swap(prev, cur);
      std::swap(prev_t, cur_t);
    }

    R_xlen_t best_segment = 0;
    for (R_xlen_t j = 1; j < n_segments; j++) {
      if (prev[j] < prev[best_segment] - tie_tolerance) best_segment = j;
    }

    // traceback, from the last stop to the first one

    segment[n_stops - 1] = best_segment;
    for (R_xlen_t i = n_stops - 1; i > 0; i--) {
      segment[i - 1] = back[
        (size_t) i * (size_t) n_segments + (size_t) segment[i]
      ];
    }
  }

  // positions along the shape. a stop projected onto the same segment as the
  // previous stop, but before it, is moved to the previous stop's position, so
  // distances between consecutive stops are never negative

  double previous_pos = 0.0;

  for (R_xlen_t i = 0; i < n_stops; i++) {
    const R_xlen_t j = segment[i];
    project(i, j, t);
    double pos = cum_dist[j] + t * seg_length[j];
    if (i > 0 && pos < previous_pos) pos = previous_pos;
    position[i] = pos;
    previous_pos = pos;
  }

  return dp_runs;
}

//' cpp_locate_stops_on_shapes
//'
//' Locates each pattern's sequence of stops along its shape, returning the distance, in
//' meters, from the start of the shape to the point of the shape where each
//' stop is projected. Each stop is projected onto the line segments of the
//' shape (not only onto its vertices), and the stops are forced to advance
//' along the shape in the order they are given, so that loops and shapes that
//' pass by the same place more than once are handled correctly.
//'
//' The stops are placed, with dynamic programming, so as to (approximately)
//' minimize the sum of the squared distances between the stops and their
//' points on the shape, under the ordering constraint. A stop located before
//' the previous one on the same segment is placed at the previous stop's
//' position (the previous stop is never moved back to make room for it).
//' Squared distances are calculated in a local
//' equirectangular frame of each shape segment, while the lengths along the
//' shape are great-circle distances calculated with the haversine formula.
//'
//' With 'prune', the dynamic program is first run only on the segments near
//' each stop, which is checked to give the same results as running it on all
//' segments (see locate_stops_on_shape() above), and the result gets a
//' "dp_runs" attribute with, for each pattern, the number of runs of the
//' pruned dynamic program needed, or 0 where the full one was used. The
//' results are identical to those without 'prune' when both use the same
//' floating-point operations, which the tests check, on constructed cases,
//' on each platform they run on. Compilers that fuse multiplications and
//' additions differently in the two dynamic programs (e.g. GCC with
//' -ffp-contract=fast on aarch64) could break ties differently, but this
//' hasn't been seen.
//'
//' The shapes' points are in 'shape_lat' and 'shape_lon', contiguous and in the
//' order of 'shape_size'. The stops of each pattern are in 'stop_lat' and
//' 'stop_lon', contiguous and in the order of 'pattern_size', and the shape of
//' each pattern is given by 'pattern_shape' (1-based). Returns the positions of
//' all stops, in the same order. Patterns are processed in parallel.
//'
//' All coordinates must be non-missing, and the shapes used must have at least
//' two points.
//'
//' @noRd
[[cpp11::register]]
doubles cpp_locate_stops_on_shapes(const doubles shape_lat,
                                   const doubles shape_lon,
                                   const integers shape_size,
                                   const integers pattern_shape,
                                   const doubles stop_lat,
                                   const doubles stop_lon,
                                   const integers pattern_size,
                                   const bool prune) {
  const R_xlen_t n_points = shape_lat.size();
  const R_xlen_t n_shapes = shape_size.size();
  const R_xlen_t n_stops = stop_lat.size();
  const R_xlen_t n_patterns = pattern_shape.size();

  if (shape_lon.size() != n_points || stop_lon.size() != n_stops) {
    stop("Latitude and longitude vectors must have the same length.");
  }
  if (pattern_size.size() != n_patterns) {
    stop("'pattern_shape' and 'pattern_size' must have the same length.");
  }

  // serial validation and offsets

  const int* shape_size_p = INTEGER(shape_size);
  const int* pattern_shape_p = INTEGER(pattern_shape);
  const int* pattern_size_p = INTEGER(pattern_size);

  std::vector<R_xlen_t> shape_start(n_shapes);
  R_xlen_t total_points = 0;
  for (R_xlen_t s = 0; s < n_shapes; s++) {
    if (shape_size_p[s] == NA_INTEGER || shape_size_p[s] < 0) {
      stop("'shape_size' must not include negative or NA values.");
    }
    shape_start[s] = total_points;
    total_points += shape_size_p[s];
  }
  if (total_points != n_points) {
    stop("'shape_size' must sum to the number of shape points.");
  }

  std::vector<R_xlen_t> stop_start(n_patterns);
  R_xlen_t total_stops = 0;
  for (R_xlen_t p = 0; p < n_patterns; p++) {
    const int s = pattern_shape_p[p];
    if (s == NA_INTEGER || s < 1 || s > n_shapes) {
      stop("'pattern_shape' must be valid shape indices.");
    }
    if (shape_size_p[s - 1] < 2) {
      stop("The shapes must have at least two points.");
    }
    if (pattern_size_p[p] == NA_INTEGER || pattern_size_p[p] < 0) {
      stop("'pattern_size' must not include negative or NA values.");
    }
    stop_start[p] = total_stops;
    total_stops += pattern_size_p[p];
  }
  if (total_stops != n_stops) {
    stop("'pattern_size' must sum to the number of stops.");
  }

  const double* shape_lat_p = REAL(shape_lat);
  const double* shape_lon_p = REAL(shape_lon);
  const double* stop_lat_p = REAL(stop_lat);
  const double* stop_lon_p = REAL(stop_lon);

  for (R_xlen_t k = 0; k < n_points; k++) {
    if (ISNAN(shape_lat_p[k]) || ISNAN(shape_lon_p[k])) {
      stop("Shape coordinates must not be missing.");
    }
  }
  for (R_xlen_t i = 0; i < n_stops; i++) {
    if (ISNAN(stop_lat_p[i]) || ISNAN(stop_lon_p[i])) {
      stop("Stop coordinates must not be missing.");
    }
  }

  writable::doubles position(n_stops);
  double* position_p = REAL(position);
  std::vector<int> dp_runs(n_patterns);

  // patterns are processed in blocks, so that the user can interrupt the
  // calculation between them. within a block, patterns are spread among the
  // threads dynamically, since their sizes vary a lot

  const int n_threads = n_patterns > 1 ? gtfs_threads() : 1;
  (void) n_threads;
  const R_xlen_t block_size = 256;
  int failed = 0;

  for (R_xlen_t first = 0; first < n_patterns; first += block_size) {
    check_user_interrupt();
    const R_xlen_t last = std::min(first + block_size, n_patterns);

#ifdef _OPENMP
#pragma omp parallel for schedule(dynamic, 1) num_threads(n_threads)
#endif
    for (R_xlen_t p = first; p < last; p++) {
      try {
        const R_xlen_t shape = pattern_shape_p[p] - 1;
        const R_xlen_t offset = shape_start[shape];
        dp_runs[p] = locate_stops_on_shape(
          shape_lat_p + offset,
          shape_lon_p + offset,
          shape_size_p[shape],
          stop_lat_p + stop_start[p],
          stop_lon_p + stop_start[p],
          pattern_size_p[p],
          position_p + stop_start[p],
          prune
        );
      } catch (...) {
        // exceptions can't leave a parallel region (e.g. std::bad_alloc)
#ifdef _OPENMP
#pragma omp atomic write
#endif
        failed = 1;
      }
    }

    if (failed) stop("Failed to locate the stops along the shapes.");
  }

  if (prune) {
    writable::integers runs(n_patterns);
    std::copy(dp_runs.begin(), dp_runs.end(), INTEGER(runs));
    position.attr("dp_runs") = runs;
  }

  return position;
}

//' cpp_shape_cut_segments
//'
//' Finds where parts of shapes start and end. 'lat' and 'lon' hold the points
//' of each shape, contiguous and in the order of 'shape_size'. Each part is on
//' the shape given by 'cut_shape' (1-based) and goes from 'from' to 'to',
//' meters from the start of the shape.
//'
//' Returns the cumulative distance of each point ('cum') and, for each part,
//' the segments that contain 'from' and 'to' and the range of points strictly
//' between them ('first_inner' to 'last_inner'), as 1-based indices into
//' 'lat', 'lon' and 'cum'. These are the results of findInterval() on each
//' shape's cumulative distances.
//'
//' @noRd
[[cpp11::register]]
list cpp_shape_cut_segments(const doubles lat,
                            const doubles lon,
                            const integers shape_size,
                            const integers cut_shape,
                            const doubles from,
                            const doubles to) {
  const R_xlen_t n_points = lat.size();
  const R_xlen_t n_shapes = shape_size.size();
  const R_xlen_t n_cuts = cut_shape.size();

  if (lon.size() != n_points) {
    stop("Latitude and longitude vectors must have the same length.");
  }
  if (from.size() != n_cuts || to.size() != n_cuts) {
    stop("'cut_shape', 'from' and 'to' must have the same length.");
  }

  std::vector<R_xlen_t> shape_start(n_shapes);
  R_xlen_t total = 0;
  for (R_xlen_t s = 0; s < n_shapes; s++) {
    if (shape_size[s] < 0) {
      stop("'shape_size' must not include negative or NA values.");
    }
    shape_start[s] = total;
    total += shape_size[s];
  }
  if (total != n_points) {
    stop("'shape_size' must sum to the number of points.");
  }
  if (total >= INT_MAX) stop("Too many shape points.");

  // cumulative distance of the points of each shape. the sum is kept in long
  // double and each value is stored as a double, as R's cumsum() does (unless
  // R is built with --disable-long-double), so the distances are the same as
  // those calculated in R. any difference would be at rounding level, which
  // build_shape_cuts() absorbs by clamping the interpolation factors

  writable::doubles cum(n_points);
  double* cum_p = REAL(cum);
  const double* lat_p = REAL(lat);
  const double* lon_p = REAL(lon);
  const int* shape_size_p = INTEGER(shape_size);

  // shapes are processed in parallel. each shape's sum stays within a single
  // iteration (no reduction across threads), so the results don't depend on
  // the number of threads

  check_user_interrupt();
  const int n_threads = n_shapes > 1 ? gtfs_threads() : 1;
  (void) n_threads;
  int is_finite = 1;

#ifdef _OPENMP
#pragma omp parallel for schedule(dynamic, 16) num_threads(n_threads)
#endif
  for (R_xlen_t s = 0; s < n_shapes; s++) {
    const R_xlen_t start = shape_start[s];
    const R_xlen_t end = start + shape_size_p[s];
    long double sum = 0.0L;

    for (R_xlen_t k = start; k < end; k++) {
      if (k > start) {
        sum += distance_haversine(
          lat_p[k - 1], lon_p[k - 1], lat_p[k], lon_p[k]
        );
      }
      const double value = static_cast<double>(sum);
      if (!std::isfinite(value)) {
#ifdef _OPENMP
#pragma omp atomic write
#endif
        is_finite = 0;
      }
      cum_p[k] = value;
    }
  }

  if (!is_finite) stop("Shape distances must be finite.");

  // for each part: the number of points of its shape whose cumulative distance
  // is <= from (as findInterval()), <= to, and < to (findInterval() with
  // left.open = TRUE). segments are clamped to the segments of the shape

  writable::integers from_segment(n_cuts);
  writable::integers to_segment(n_cuts);
  writable::integers first_inner(n_cuts);
  writable::integers last_inner(n_cuts);

  const double* cum_begin = cum_p;

  for (R_xlen_t i = 0; i < n_cuts; i++) {
    if (cut_shape[i] == NA_INTEGER || cut_shape[i] < 1 ||
        cut_shape[i] > n_shapes) {
      stop("'cut_shape' must be valid shape indices.");
    }
    const R_xlen_t s = cut_shape[i] - 1;

    const R_xlen_t offset = shape_start[s];
    const R_xlen_t n = shape_size[s];
    const double* first = cum_begin + offset;
    const double* last = first + n;

    const double from_i = from[i];
    const double to_i = to[i];
    if (ISNAN(from_i) || ISNAN(to_i)) {
      stop("'from' and 'to' must not be missing.");
    }
    const R_xlen_t n_up_to_from = std::upper_bound(first, last, from_i) - first;
    const R_xlen_t n_up_to_to = std::upper_bound(first, last, to_i) - first;
    const R_xlen_t n_below_to = std::lower_bound(first, last, to_i) - first;
    const R_xlen_t last_segment = n - 1;

    from_segment[i] = static_cast<int>(
      offset + std::min(std::max(n_up_to_from, (R_xlen_t) 1), last_segment)
    );
    to_segment[i] = static_cast<int>(
      offset + std::min(std::max(n_up_to_to, (R_xlen_t) 1), last_segment)
    );
    first_inner[i] = static_cast<int>(offset + n_up_to_from + 1);
    last_inner[i] = static_cast<int>(offset + n_below_to);
  }

  using namespace cpp11::literals;

  return writable::list({
    "cum"_nm = cum,
    "from_segment"_nm = from_segment,
    "to_segment"_nm = to_segment,
    "first_inner"_nm = first_inner,
    "last_inner"_nm = last_inner
  });
}
