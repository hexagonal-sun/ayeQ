#include <complex.h>

static void push(int klen,
                 float complex val,
                 float complex *state,
                 int *headPos)
{
    (*headPos)--;

    if (*headPos == -1)
        (*headPos) = klen - 1;

    state[*headPos] = val;
}

void convolve(int klen,
              int buflen,
              int *headPos,
              const float * const kernel,
              float complex *state,
              const float complex * const in,
              float complex *out)
{
    for (int sampIdx = 0; sampIdx < buflen; sampIdx++) {
        float complex ret = 0 + 0*I;

        push (klen, in[sampIdx], state, headPos);

        for (int i = 0; i < klen; i++)
            ret += kernel[i] * state[((*headPos) + i) % klen];

        out[sampIdx] = ret;
    }
}
