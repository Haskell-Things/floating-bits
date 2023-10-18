
#include <string.h>
#include <stdint.h>
#include <math.h>
#include <assert.h>

uint32_t float2word_ref(float val)
{
    uint32_t rep;
    assert(sizeof rep == sizeof val);
    memcpy(&rep, &val, sizeof rep);
    return rep;
}

float word2float_ref(uint32_t rep)
{
    float val;
    assert(sizeof rep == sizeof val);
    memcpy(&val, &rep, sizeof val);
    return val;
}

uint64_t double2word_ref(double val)
{
    uint64_t rep;
    assert(sizeof rep == sizeof val);
    memcpy(&rep, &val, sizeof rep);
    return rep;
}

double word2double_ref(uint64_t rep)
{
    double val;
    assert(sizeof rep == sizeof val);
    memcpy(&val, &rep, sizeof val);
    return val;
}

// ulp reference implementation copied from java

#define DOUBLE_MIN_VALUE 4.94065645841246544176568792868E-324
#define DOUBLE_SIGNIFICAND_WIDTH 53L
#define DOUBLE_MAX_EXPONENT 1023L
#define DOUBLE_MIN_EXPONENT -1022L
#define DOUBLE_EXP_BIAS 1023L
#define DOUBLE_EXP_BIT_MASK 0x7FF0000000000000L

#define FLOAT_MIN_VALUE 1.40129846432481707092372958329E-45
#define FLOAT_SIGNIFICAND_WIDTH 24
#define FLOAT_MAX_EXPONENT 127
#define FLOAT_MIN_EXPONENT -126
#define FLOAT_EXP_BIAS 127
#define FLOAT_EXP_BIT_MASK 0x7F800000

static int64_t get_exponent_d(double d)
{
    return (int64_t)(((double2word_ref(d) & DOUBLE_EXP_BIT_MASK) >> (DOUBLE_SIGNIFICAND_WIDTH - 1L)) - DOUBLE_EXP_BIAS);
}

static int32_t get_exponent_f(float f)
{
    return (int32_t)((float2word_ref(f) & FLOAT_EXP_BIT_MASK) >> (FLOAT_SIGNIFICAND_WIDTH - 1)) - FLOAT_EXP_BIAS;
}

static double power_of_two_d(int64_t n)
{
    assert((n >= DOUBLE_MIN_EXPONENT) && (n <= DOUBLE_MAX_EXPONENT));
    return word2double_ref(((uint64_t)(n + DOUBLE_EXP_BIAS) << (DOUBLE_SIGNIFICAND_WIDTH - 1L)) & DOUBLE_EXP_BIT_MASK);
}

static float power_of_two_f(int32_t n)
{
    assert((n >= FLOAT_MIN_EXPONENT) && (n <= FLOAT_MAX_EXPONENT));
    return word2float_ref(((uint32_t)(n + FLOAT_EXP_BIAS) << (FLOAT_SIGNIFICAND_WIDTH - 1)) & FLOAT_EXP_BIT_MASK);
}

float float_ulp_ref(float f)
{
    int32_t e = get_exponent_f(f);

    switch(e) {
    case FLOAT_MAX_EXPONENT+1:        // NaN or infinity
        return fabsf(f);

    case FLOAT_MIN_EXPONENT-1:        // zero or subnormal
        return FLOAT_MIN_VALUE;

    default:
        assert((e <= FLOAT_MAX_EXPONENT) && (e >= FLOAT_MIN_EXPONENT));

        // ulp(x) is usually 2^(SIGNIFICAND_WIDTH-1)*(2^ilogb(x))
        e = e - (FLOAT_SIGNIFICAND_WIDTH - 1);
        if (e >= FLOAT_MIN_EXPONENT) {
            return power_of_two_f(e);
        }
        else {
            // return a subnormal result; left shift integer
            // representation of FLOAT_MIN_VALUE appropriate
            // number of positions
            return word2float_ref(1 << (e - (FLOAT_MIN_EXPONENT - (FLOAT_SIGNIFICAND_WIDTH - 1))));
        }
    }
}

double double_ulp_ref(double d)
{
    int64_t e = get_exponent_d(d);

    switch(e) {
    case DOUBLE_MAX_EXPONENT+1:       // NaN or infinity
        return fabs(d);

    case DOUBLE_MIN_EXPONENT-1:       // zero or subnormal
        return DOUBLE_MIN_VALUE;

    default:
        assert((e <= DOUBLE_MAX_EXPONENT) && (e >= DOUBLE_MIN_EXPONENT));

        // ulp(x) is usually 2^(SIGNIFICAND_WIDTH-1)*(2^ilogb(x))
        e = e - (DOUBLE_SIGNIFICAND_WIDTH - 1);
        if (e >= DOUBLE_MIN_EXPONENT) {
            return power_of_two_d(e);
        }
        else {
            // return a subnormal result; left shift integer
            // representation of Double.MIN_VALUE appropriate
            // number of positions
            return word2double_ref(1L << (e - (DOUBLE_MIN_EXPONENT - (DOUBLE_SIGNIFICAND_WIDTH - 1))));
        }
    }
}
