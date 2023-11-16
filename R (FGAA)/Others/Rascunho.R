
# hum ---------------------------------------------------------------------
do.call(function(a, b) function(x){ifelse(x == 1, 0, a/(x-1)^b)}, prob_args)

# Criar mensagem relembrando do forms -------------------------------------
link_forms = 
  sheet_forms = read_sheet(link_forms) %>%
  transmute(Nome = str_to_title(`Nome completo:`),
            Carteira = gsub(".*([0-9]{6})", "C\\1", `Número da carteirinha (Cxxxxxx):`))

sheet_aprovados = left_join(read_sheet(link_alocacao, sheet = "Aprovados (email)"), sheet_alocacao, by = "Nome")


faltantes = sheet_aprovados %>% 
  filter(! Carteira %in% sheet_forms$Carteira) %>% #filtrar quem já respondeu
  select(c(Nome, Carteira)) %>%
  arrange(Nome) %>% #reordenar o dataset para melhor vizualização
  print(n = nrow(.))

sheet_inscricao %>%
  filter(Carteira %in% faltantes$Carteira)
#Boa noite Laís, tudo bem? Viu o email que te enviamos?
#Perdão pelo horário, mas precisamos que você siga as instruções lá contidas até hoje. Obrigado!




link_alocacao = "https://docs.google.com/spreadsheets/d/14kezubt6lEWWYo33OC9gp0G4CIKgOfZuXRwjnzVI6UM/edit?usp=sharing"
sheet_alocacao = read_sheet(link_alocacao, sheet = "Forms") %>%
  formatar_colunas() %>%
  rename_with(~ gsub(".+\\[(.+)\\]", "\\1", .x), -c(Nome, Carteira))

preferencias <- sheet_alocacao %>%
  select(-c(Carimbo, Nome)) %>%
  set_names(c("Carteira", paste0("Opcao", 1:(ncol(.)-1)))) %>%
  mutate(across(-Carteira, ~ as.numeric(str_extract(.x, "[0-9]+"))))

ngrupos <- ceiling(nrow(preferencias)/4)
opcoes <- preferencias[-1] %>%
  replace(is.na(.), ncol(preferencias)+1) %>%
  map_dbl(~ round(mean(.x),1)) %>%
  {names(.[order(.)])[1:ngrupos]}

penal <- ncol(preferencias)+1
preferencias <- preferencias %>%
  select(c(Carteira, all_of(opcoes))) %>%
  replace(is.na(.), penal)


eval_fun <- function(ordem, preferencias){
  preferencias <- split(preferencias, seq(nrow(preferencias)))
  alocacao <- ceiling(ordem/4)
  
  custos <- pmap_dbl(list(preferencias, alocacao), ~ as.numeric(.x[.y+1]))
  sum(custos^2)
}

ordem <- sample(1:nrow(preferencias), nrow(preferencias))
eval_fun(ordem, preferencias)

GA <- ga(type = "permutation",
         fitness = eval_fun, preferencias = preferencias,
         crossover = GA::gaperm_cxCrossover, #alterar
         lower = 1, upper = nrow(preferencias),
         popSize = pop <- 100, #tamanho da populacao
         pcrossover = 0.8, #taxa de crossover
         pmutation = 0.40, #taxa de mutacao
         elitism = 0.05 * pop, #taxa de sobrevivencia
         run = 100, #runs ate estabelecer convergencia
         maxiter = 5000,
         parallel = TRUE)
summary(GA)

sol = GA@solution
ceiling(GA@solution/4)[1,]





if(prob_fun = "linear"){
  if(is.null(prob_args)) prob_args <- list(a = 0.6, b = 2)
  prob_fun <- do.call(function(a, b) function(x){1 - a - a*b*x/max(x)}, prob_args)
}



# listed population -------------------------------------------------------
fgga_populate <- function(n_sizes, n_pop, sizes_base, preferences, suggestions = NULL,
                          n_opt = ncol(preferences), n_obs = nrow(preferences)){
  
  population <- replicate(n_sizes,
                          list(pop = matrix(nrow = n_obs, ncol = n_pop),
                               obj = double(n_pop)), 
                          simplify = FALSE)
  
  if(is.null(suggestions)){
    sizes_chosen <- replicate(n_sizes, sample(sizes_base))
  } else {
    sizes_chosen <- c(suggestions, replicate(n_sizes - length(suggestions), sample(sizes_base)))
  }
  
  for(s in 1:n_sizes){
    population[[s]]$sizes <- sizes_chosen[,s]
    for(i in 1:n_pop){
      population[[s]]$pop[,i] <- sample(rep(1:n_opt, times = population[[s]]$sizes))
      population[[s]]$obj[i] <- obj(population[[s]]$pop[,i], preferences, n_obs)
    }
  }
  
  return(population)
}


fgga_pop_crossover <- function(population, sizes_chosen, n_pop, n_obs, n_opt, preferences,
                               elitism = 0.25*n_pop, n_parents = 2, max_parents = n_parents){
  objs <- sapply(population, function(x) x$obj)
  parents <- population$pop[, which(objs >= sort(objs, decreasing = TRUE)[elitism])]
  
  children <- list(pop = matrix(nrow = n_obs, ncol = n_pop),
                   sizes = NULL,
                   obj = matrix(nrow = n_obs, ncol = n_pop))
  
  for(i in 1:n_pop){
    parents_chosen <- parents[,sample(1:ncol(parents), n_parents)]
    parents_options <- cbind(obs = rep(1:n_obs, n_parents),
                             alloc = as.double(parents_chosen),
                             obj = as.double(apply(parents_chosen, 2, obj, n_obs = n_obs,
                                                   preferences = preferences))) %>%
      `colnames<-`(c("obs", "option", "obj"))
    
    child <- select_recursion(parents_options, k = n_obs, max = max_parents,
                              sizes = rep(1:n_opt, times = sizes_chosen))
    
    child[is.na(child)] <- resample(rep(1:n_opt, times = sizes_chosen - tabulate_all(child, n_opt)))
    children$pop[,i] <- child
    children$obj[,i] <- obj(child, n_obs, prefs)
  }
  
  return(children)
}

fgga_sizes_crossover <- function(population, sizes_base,
                                 n_opt = length(sizes_base), n_pop = ncol(population[[1]])){
  obj_merged <- do.call(c, lapply(population, `[[`, "obj"))
  sizes_merged <- do.call(cbind, lapply(population, `[[`, "sizes"))
  
  sizes_unique <- unique(sizes_base)
  sizes_length <- length(sizes_unique)
  
  sizes_options <- matrix(nrow = n_opt*sizes_length, ncol = 3) %>%
    `colnames<-`(c("obs", "option", "obj"))
  
  for(opt in 1:n_opt){
    for(i in 1:sizes_length){
      index <- vapply(n_pop * (which(sizes_merged[opt,] == sizes_unique[i]) - 1),
                      function(x) 1:n_pop + x, double(n_pop))
      sizes_options[(opt-1)*3 + i,] <- c(opt, sizes_unique[i], remean(obj_merged[index]))
    }
  }
  
  select_recursion(sizes_options, k = n_opt, sizes = sizes_base)
}


# Packages and functions --------------------------------------------------
library(tidyverse)
#library(googlesheets4)
#library(GA)
#library(parallel)
#library(doParallel)


deal_base_sizes <- function(n_obs, n_opt, init_size = 5){
  rest <- n_obs %% init_size
  
  rep(init_size, n_obs %/% init_size) %>%
    {if(rest == 4){
      c(., 4)
    } else {
      . + rep(1:0, times = c(rest, n_obs %/% init_size - rest))
    }} %>%
    c(rep(0, n_opt - length(.)))
}


tabulate_all <- function(x, k) c(tabulate(x), rep(0, k - max(x, na.rm = TRUE)))


obj <- function(allocation, n_obs, prefs){prefs[n_obs*(allocation-1) + 1:n_obs]}


initial_population <- function(n_pop, n_obs, n_opt, prefs, sizes = base_sizes, suggestions = NULL){
  population <- list(pop = matrix(nrow = n_obs, ncol = n_pop),
                     obj = matrix(nrow = n_obs, ncol = n_pop),
                     sizes = matrix(nrow = length(sizes), ncol = n_pop))
  
  if(!is.null(suggestions)){
    n_sug <- ncol(suggestions)
    population$pop[,1:n_sug] <- suggestions
    population$sizes[,1:n_sug] <- apply(suggestions, 2, tabulate_all, k = n_opt)
    population$obj[,1:n_sug] <- apply(suggestions, 2, obj, n_obs = n_obs, prefs = prefs)
  } else {
    n_sug <- 0
  }
  
  for(s in 1:n_sizes){
    
  }
  
  for(i in (n_sug+1):n_pop){
    population$sizes[,i] <- sample(sizes)
    population$pop[,i] <- sample(rep(1:n_opt, times = population$sizes[,i]))
    population$obj[,i] <- obj(population$pop[,i], n_obs = n_obs, prefs = prefs)
  }
  
  return(population)
}


select_recursion <- function(x, k, sizes, max = nrow(x)/k){
  check_groups <- function(ind, result, sizes){
    x[ind, "option"] %>% {sum(result == ., na.rm = TRUE) < sum(sizes == .)}
  }
  
  recursion <- function(i, x, index, result, sizes){ #se o 1st best ta cheio, procure no 2nd, etc
    if(check_groups(index[i], result, sizes)){
      x[index[i], "option"]
    } else if(i+1 <= max){
      recursion(i+1, x, index, result, sizes) 
    } else {
      NA
    }
  }
  # daria pra fazer: se o 1st best está cheio, va para a proxima obs. Apos preencher os 1sts,
  # volte do inicio olhandopros 2nds pra preencher o que sobrou, etc
  
  x <- x[order(x[,"obj"]),] #se empatra no order, escolhe aleatorio
  result <- rep(NA, k)
  i <- 1
  
  for(obs in unique(x[,"obs"])){
    index <- which(x[,"obs"] == obs)
    result[obs] <- recursion(i, x, index, result, sizes)
  }
  
  return(result)
}


MFH_sizes_selection <- function(sizes, population, n_obs, n_opt){
  unique_sizes <- unique(sizes)
  n_sizes <- length(unique_sizes)
  
  sizes_options <- matrix(nrow = n_opt*n_sizes, ncol = 3) %>%
    `colnames<-`(c("obs", "option", "obj"))
  
  for(opt in 1:n_opt){
    for(i in 1:n_sizes){
      mean_obj <- mean(population$obj[population$sizes[opt,] == unique_sizes[i]])
      sizes_options[(opt-1)*3 + i,] <- c(opt, unique_sizes[i], mean_obj)
    }
  }
  
  select_recursion(sizes_options, k = n_opt, sizes = base_sizes)
}


MFH_sizes_mutation <- function(sizes, n_opt, prob_fun = "inverse", prob_args = NULL){
  if(prob_fun == "inverse"){
    if(is.null(prob_args)) prob_args <- list(a = 0.2, b = 5)
    prob_fun <- do.call(function(a, b) function(x){ifelse(x == 1, 0, a/(x-1)^b)}, prob_args)
  }
  
  n_mutations <- sample(1:n_opt, 1, prob = prob_fun(1:n_opt))
  index <- sample(1:n_opt, n_mutations)
  
  sizes[index] <- sample(sizes[index])
  return(sizes)
}


MFH_pop_crossover <- function(population, sizes, n_pop, n_obs, n_opt, prefs,
                              elitism = 0.25*n_pop, n_parents = 2, max_parents = n_parents){
  resample <- function(x, ...) x[sample.int(length(x), ...)]
  
  objs <- apply(population$obj, 2, mean)
  parents <- population$pop[, which(objs >= sort(objs, decreasing = TRUE)[elitism])]
  
  children <- list(pop = matrix(nrow = n_obs, ncol = n_pop),
                   sizes = NULL,
                   obj = matrix(nrow = n_obs, ncol = n_pop))
  
  for(i in 1:n_pop){
    parents_chosen <- parents[,sample(1:ncol(parents), n_parents)]
    
    parents_options <- cbind(obs = rep(1:n_obs, n_parents),
                             alloc = as.double(parents_chosen),
                             obj = as.double(apply(parents_chosen, 2, obj, n_obs = n_obs, prefs = prefs))) %>%
      `colnames<-`(c("obs", "option", "obj"))
    
    child <- select_recursion(parents_options, k = n_obs, max = max_parents,
                              sizes = rep(1:n_opt, times = sizes))
    
    child[is.na(child)] <- resample(rep(1:n_opt, times = sizes - tabulate_all(child, n_opt)))
    children$pop[,i] <- child
    children$obj[,i] <- obj(child, n_obs, prefs)
  }
  
  return(children)
}





# Hmmmmm ------------------------------------------------------------------
set.seed(1)
n_obs <- 61; n_opt <- 14; n_pop = 60; prefs <- t(replicate(n_obs, sample(1:n_opt, n_opt)))

#mandioquinha_frita_hmm <- function(data, n_pop){ #
n_obs <- nrow(prefs)
n_opt <- ncol(prefs)

base_sizes <- deal_base_sizes(n_obs, n_opt)

population <- initial_population(n_pop, n_obs, n_opt, prefs = prefs, sizes = base_sizes)
chosen_sizes <- MFH_sizes_selection(base_sizes, population, n_obs, n_opt)
mean(apply(population$obj, 2, sum))
mean(population$obj)

population <- MFH_pop_crossover(population, chosen_sizes, n_pop, n_obs, n_opt, prefs, n_parents = 4)
for(i in 1:10){
  population <- MFH_pop_crossover(population, chosen_sizes, n_pop, n_obs, n_opt, prefs, n_parents = 4)
  cat(paste0("====== ", i, " ======\n",
             round(mean(apply(population$obj, 2, sum)), 1),
             round(mean(population$obj), 2),
             "\n"))
}

#population <- MFH_pop_crossover(population, chosen_sizes, n_pop, n_obs, n_opt, prefs, n_parents = 2)
#mean(apply(population$obj, 2, sum))
#mean(population$obj)
#}


#memmoise
