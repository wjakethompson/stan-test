library(tidyverse)
library(rstan)
library(loo)
library(tidybayes)

### Generate data --------------------------------------------------------------
set.seed(9416)
items <- tibble(item_id = seq_len(20)) %>%
  mutate(int = runif(n(), -2.50, -1.00),
         mef = runif(n(), 1.00, 4.50),
         nm_prob = 1 / (1 + exp(-1 * int)),
         ms_prob = 1 / (1 + exp(-1 * (int + mef))))

resps <- crossing(resp_id = seq_len(2000), item_id = seq_len(20)) %>%
  mutate(class = sample(c(0, 1), n(), replace = TRUE, prob = c(0.4, 0.6))) %>%
  left_join(items, by = "item_id") %>%
  mutate(prob = case_when(class == 0 ~ nm_prob,
                          class == 1 ~ ms_prob),
         rand = runif(n(), 0, 1),
         score = case_when(rand <= prob ~ 1L, TRUE ~ 0L))

scores <- resps %>%
  select(resp_id, item_id, score) %>%
  arrange(resp_id, item_id)

ragged_array  <- scores %>%
  rowid_to_column() %>%
  group_by(resp_id) %>%
  summarize(start = min(rowid), num = n())

stan_data <- list(
  I = 20,
  J = 2000,
  N = 40000,
  C = 2,
  A = 1,
  ii = scores$item_id,
  jj = scores$resp_id,
  y = scores$score,
  s = ragged_array$start,
  l = ragged_array$num,
  Alpha = matrix(data = c(0, 1), ncol = 1)
)

jiang <- stan("lcdm-jiang.stan", data = stan_data, chains = 4, iter = 2000, cores = 4)
