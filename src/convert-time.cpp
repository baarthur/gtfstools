#include "convert-time.h"

#include <climits>

namespace {

inline bool is_space(const char c) {
    return c == ' ' || c == '\t' || c == '\r' || c == '\n';
}

// parses "H:MM:SS" in place, without copying the string. surrounding
// whitespace is skipped; malformed strings return NA_INTEGER
int convert_time_to_seconds(const char* hms) {
    const char* p = hms;
    while (is_space(*p)) p++;

    long long part[3] = {0, 0, 0};
    int ndig[3] = {0, 0, 0};
    int field = 0;
    for (; *p != '\0' && !is_space(*p); p++) {
        const char c = *p;
        if (c == ':') {
            if (++field > 2) return NA_INTEGER;
        } else if (c >= '0' && c <= '9' && ndig[field] < 9) {
            part[field] = part[field] * 10 + (c - '0');
            ndig[field]++;
        } else {
            return NA_INTEGER;
        }
    }
    while (is_space(*p)) p++;
    if (*p != '\0') return NA_INTEGER;

    if (field != 2 || ndig[0] == 0 || ndig[1] != 2 || ndig[2] != 2 ||
        part[1] >= 60 || part[2] >= 60) {
        return NA_INTEGER;
    }

    const long long secs = 3600 * part[0] + 60 * part[1] + part[2];
    if (secs > INT_MAX) return NA_INTEGER;
    return static_cast<int>(secs);
}

} // namespace

//' cpp_time_to_seconds
//'
//' Vectorize the above function
//'
//' @noRd
[[cpp11::register]]
integers cpp_time_to_seconds(const strings times_in) {
    const R_xlen_t n = times_in.size();
    writable::integers res(n);

    for (R_xlen_t i = 0; i < n; i++) {
        const SEXP time = STRING_ELT(times_in, i);
        res[i] = time == NA_STRING ? NA_INTEGER :
            convert_time_to_seconds(CHAR(time));
    }

    return res;
}
