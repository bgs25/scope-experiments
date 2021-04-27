library(CatReg)


TimeMyCode = function( gammaseq, lambdaseq ) {

  category.noise = function( discrete_df, nmult ) {
    for ( j in 1:dim(discrete_df)[ 2 ] ) {
      print(paste0("Variable = ", j))
      nlevels = length(levels(discrete_df[ , j ]))
      for ( i in 1:((nmult-1) * nlevels) ) {
        which.level = sample(levels(discrete_df[ , j ]), prob= table(discrete_df[ , j ]), 1)
        discrete_df[ , j ] = divide.level(discrete_df[ , j ], which.level)
      }

    }
    return(discrete_df)
  }


  divide.level = function( data.vector, which.level ) {
    data.vector = as.character(data.vector)
    for (i in 1:length(data.vector)) {
      if ( data.vector[i] == which.level ) {
        if ( rbinom(1,1,0.5) == 1 ) {
          data.vector[i] = paste0(which.level, "-0")
        } else {
          data.vector[i] = paste0(which.level, "-1")
        }
      }
    }
    data.vector = as.factor(data.vector)
    return(data.vector)
  }

  noiseseq = c(1, seq(5, 80, 5))
  time_matrix = list()
  for ( setting in 1:3 ) {
    time_matrix[[ setting ]] = list()
    for ( j in 1:length(gammaseq) ) {
      time_matrix[[ setting ]][[ j ]] = list()
      for ( k in 1:length(lambdaseq) ) {
        time_matrix[[ setting ]][[ j ]][[ k ]] = matrix(0, 25, 14)
      }
    }
  }


  occupation = fulldsc$occupation
  for ( rep in 1:25 ) {
    print(paste0("Rep: ", rep))
    vars = occupation
    signal = matrix(0, 14, 3)
    signal[ , 2 ] = sample(c(rep(-2, 7), rep(2, 7)))
    signal[ , 3 ] = sample(c(rep(-2, 3), rep(-1, 3), rep(0, 2), rep(1, 3), rep(2, 3)))

    response = matrix(0, 45222, 3)
    response[ , 2 ] = signal[ occupation, 2 ]
    response[ , 3 ] = signal[ occupation, 3 ]

    response[ , 2 ] = response[ , 2 ] * sqrt(45222) / sqrt(sum(response[ , 2 ]^2))
    response[ , 3 ] = response[ , 3 ] * sqrt(45222) / sqrt(sum(response[ , 3 ]^2))

    # response = response + matrix(10 * rnorm(45222 * 3), 45222, 3)
    for ( catnoise in 1:14 ) {
      print(paste0("Noise level: ", catnoise))
      if ( catnoise > 1 ) {
        vars = data.frame(vars)
        vars = category.noise(vars, noiseseq[ catnoise ] / noiseseq[ catnoise - 1 ])
        vars = as.factor(vars[ , 1 ])
      }
      for ( setting in 1:3 ) {
        tw = table(vars) / 45222
        tr = tapply(response[ , setting ], vars, mean)[ levels(vars) ]
        tr = tr + sqrt(0.1) * rnorm(length(tr))
        print(paste0("Actual multiple of categories: ", length(tr) / 14))
        for ( j in 1:length(gammaseq) ) {
          for ( k in 1:length(lambdaseq) ) {
              time_matrix[[ setting ]][[ j ]][[ k ]][ rep, catnoise ] = system.time(CatReg:::DoBlock(tw, tr, gammaseq[ j ], lambdaseq[ k ]))[ 3 ]
          }
        }

      }
    }
    save(time_matrix, file="Section_S_1_1_results.Rdata")
  }
  return(time_matrix)
}
