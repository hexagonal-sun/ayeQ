#include <complex.h>

static void rotate (int klen, float complex *state)
{
    for (int i = klen - 1; i >= 1; i--)
        state[i] = state [i-1];
}

void convolve(int klen,
              int buflen,
              const float complex * const kernel,
              float complex *state,
              const float complex * const in,
              float complex *out)
{
    for (int sampIdx = 0; sampIdx < buflen; sampIdx++) {
        float complex ret = 0 + 0*I;

        rotate(klen, state);
        state[0] = in[sampIdx];

        for (int i = 0; i < klen; i++)
            ret += kernel[i] * state[i];

        out[sampIdx] = ret;
    }
}
