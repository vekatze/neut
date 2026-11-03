#include <stdint.h>

int add_const(int value) {
  return value + 100;
}

int64_t neut_test_double(int64_t value);
double neut_test_half(double value);
int32_t neut_test_decrement(int32_t value);
int neut_test_increment(int value);
int32_t neut_test_peek(const void *pointer);
void neut_test_record(int64_t value);

static int64_t recorded;

int64_t call_double(int64_t value) {
  return neut_test_double(value);
}

int64_t call_half(void) {
  return (int64_t)(neut_test_half(7.0) * 10);
}

int32_t call_decrement(int32_t value) {
  return neut_test_decrement(value);
}

int64_t call_increment(void) {
  return neut_test_increment(-3);
}

int64_t call_peek(void) {
  const int32_t values[] = {-7, 1};
  return neut_test_peek(values);
}

void note(int64_t value) {
  recorded = value * 3;
}

int64_t call_record(int64_t value) {
  neut_test_record(value);
  return recorded;
}
