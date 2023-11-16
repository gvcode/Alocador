obj1 <- function(){
  fitness <- double(n_obs)
  for(i in 1:n_obs){
    fitness[i] <- data[i, allocation[i]]}
  sum(fitness)
}

obj2 <- function(){
  fitness <- 0
  for(i in 1:n_obs){
    fitness <- fitness + data[i, allocation[i]]}
  fitness
}

obj3 <- function(){
  sum(data[cbind(1:nrow(data),allocation)])
}

obj4 <- function(){
  sum(t(data)[allocation + n_opt*(0:(n_obs-1))])
}

obj5 <- function(){
  sum(data[n_obs*(allocation-1) + 1:n_obs])
}


n_obs <- 61; n_opt <- 14; data <- t(replicate(n_obs, sample(1:n_opt, n_opt)))
base_sizes <- deal_base_sizes(n_obs, n_opt)

chosen_sizes <- sample(base_sizes)
allocation <- sample(rep(1:length(chosen_sizes), times = chosen_sizes))

bench::mark(obj1(), obj2(), obj3(), obj4(), obj5())


#################
v = population[,53]
bench::mark(table(v), collapse::fcount(v), collapse::qtab(v),
            cbind(tabulate(v), 1:max(v)),
            c(tabulate(v), rep(0, n_opt - max(v))), check = FALSE)



#################
select_recursion <- function(x, max, sizes = base_sizes){
  check_groups <- function(ind, result, sizes){
    x[ind, "option"] %>% {sum(result == ., na.rm = TRUE) < sum(sizes == .)}
  }
  
  recursion <- function(i, x, index, result, sizes){
    if(check_groups(index[i], result, sizes)){
      x[index[i], "option"]
    } else if(i+1 <= max){
      recursion(i+1, x, index, result, sizes)
    } else {
      NA
    }
  }
  
  x <- x[order(x[,"obj"]),]
  result <- rep(NA, n_opt)
  i <- 1
  
  for(obs in unique(x[,"obs"])){
    index <- which(x[,"obs"] == obs)
    result[obs] <- recursion(i, x, index, result, sizes)
  }
  
  return(result)
}


select_recursion2 <- function(x, max, sizes = base_sizes){
  check_groups <- function(ind, result, sizes){
    x[ind, "option"] %>% {sum(result == ., na.rm = TRUE) < sum(sizes == .)}
  }
  
  recursion <- function(x, index, result, sizes){
    for(i in 1:max){
      if(check_groups(index[i], result, sizes)){
        return(x[index[i], "option"])
      }
    }
    return(NA)
  }
  
  x <- x[order(x[,"obj"]),]
  result <- rep(NA, n_opt)
  
  for(obs in unique(x[,"obs"])){
    index <- which(x[,"obs"] == obs)
    result[obs] <- recursion(x, index, result, sizes)
  }
  
  return(result)
}

bench::mark(select_recursion(sizes_options, max = 2),
            select_recursion2(sizes_options, max = 2))



# rascunhos ---------------------------------------------------------------


check_groups <- function(index) x[index, 2] %>% {sum(child == .) < table(base_sizes)[paste(.)]}

child <- double(n_opt)
for(i in unique(x[,1])){
  ind = which(x[,1] == i)
  if(check_groups(ind[1])){
    child[i] <- x[ind[1], 2]
  } else if(check_groups(ind[2])){
    child[i] <- x[ind[2], 2]
  } else if(check_groups(ind[3])){
    child[i] <- x[ind[3], 2]
  }
  
  
  
  
}




tabulate(population[,1])


#nome = c("Aline", "Arthur", "Bruna", "Caio", "Célika", "Diego", "Douglas", "Ellen", "Enzo",
#         "Eric", "Felipe", "Gabriel", "Giovanna", "Guilherme", "Henrique", "Jessica", "João",
#         "Jorge", "Laís", "Leonardo", "Luis", "Luisa", "Maria", "Maycon", "Paula", "Vitor")
n_obs <- 61; n_opt <- 14; data <- initial_population(n_obs, n_opt)

n_obs <- nrow(data)
n_opt <- ncol(data)
base_sizes <- deal_base_sizes(n_obs, n_opt)


chosen_sizes <- sample(base_sizes)
groups_pool <- rep(1:length(chosen_sizes), times = chosen_sizes)


allocation <- sample(groups_pool)
obj(allocation, data) %>% sum()


#crossover
parent1 <- sample(groups_pool); sum(obj(parent1, data))
parent2 <- sample(groups_pool); sum(obj(parent2, data))

x <- cbind(obs = rep(1:n_obs, 2),
           alloc = c(parent1, parent2),
           obj = c(obj(parent1, data), obj(parent2, data))) %>%
  {.[order(.[,"obj"]),]}

child <- double(n_obs)
for(i in unique(x[,"obs"])){
  ind = which(x[,"obs"] == i)
  if(check_groups(ind[1])){
    child[i] <- x[ind[1], "alloc"]
  } else if(check_groups(ind[2])){
    child[i] <- x[ind[2], "alloc"]
  }
}

groups_pool_rest <- rep(1:length(chosen_sizes), times = chosen_sizes - tabulate(child))
chosen_rest <- sample(groups_pool_rest)
child[child == 0] <- chosen_rest

sum(obj(child, data))



n_obs <- 61; n_opt <- 14; data <- initial_population(n_obs, n_opt)

