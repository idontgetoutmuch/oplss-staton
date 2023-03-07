import sys
from torch.distributions import Uniform

from infer import run_inference, run_inference_icml2022
from ppl import ProbCtx, run_prob_prog, ProbRun, T

import numpy as np

def geometric(ctx: ProbCtx) -> int:
    """Describes a geometric distribution"""
    sample = ctx.sample(Uniform(0.0, 1.0), is_cont=False)
    ctx.constrain(sample, 0.0, 1.0)
    if sample < 0.2:
        return 1
    else:
        return 1 + geometric(ctx)


count = 100
rep = 1729
samples = run_inference(
    lambda trace: run_prob_prog(geometric, trace=trace),
    name=f"geometric{rep}",
    count=count,
    burnin=100,
    leapfrog_steps=5,
    eps=0.1,
    seed=rep,
)

m = np.mean(samples['hmc']['samples'])
