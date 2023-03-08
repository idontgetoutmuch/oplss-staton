import sys
from torch.distributions import Normal, Uniform

from infer import run_inference, run_inference_icml2022
from ppl import ProbCtx, run_prob_prog, ProbRun, T

import numpy as np

mu0 = 0.0
sigma0 = 1.0
sigma = 1.0
z = 0.0

def analytic_example(ctx: ProbCtx) -> float:
    """An analytic example"""
    mu = ctx.sample(Normal(mu0, sigma0), is_cont=True)
    ctx.observe(torch.tensor(z, requires_grad=True), Normal(mu, sigma))
    return mu

count = 100
rep = 1729
samples = run_inference(
    lambda trace: run_prob_prog(analytic_example, trace=trace),
    name=f"analytic{rep}",
    count=count,
    burnin=100,
    leapfrog_steps=5,
    eps=0.1,
    seed=rep,
)

result = [t.item() for t in samples['hmc']['samples']]

estimated_mean = np.mean(result)

estimated_sigma = np.sqrt(np.var(result))

true_mean = z * sigma0**2 / (sigma**2 + sigma0**2) + mu0 * sigma**2 / (sigma**2 + sigma0**2)
true_sigma = np.sqrt(1 / (1 / sigma0**2 + 1 / sigma**2))
