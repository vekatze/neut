#include <stdint.h>

int add_const(int value) {
  return value + 100;
}

int64_t neut_test_double(int64_t value);

int64_t call_double(int64_t value) {
  return neut_test_double(value);
}
