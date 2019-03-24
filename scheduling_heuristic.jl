using JuMP
using CPLEX


#####################      HEURISTIC PARAMETERS     #######################

###############################################################################
### THESE ARE THE DEFAULT VALUES. THEY CAN BE CHANGED IN ANY PART OF THE CODE
###############################################################################
MAX_ITERATIONS_WITHOUT_IMPROVEMENT = 50
MAX_EXECUTION_TIME = 60*30
CONSTRUCTIVE_HEURISTIC_STRATEGY = 1
SWAP_UPDATE_STRATEGY = 1  # 1 = ALWAYS THE SAME CURRENT SOLUTION
                          # 2 = UPDATE CURRENT SOLUTION IN EACH MACHINE
                          # 3 = UPDATE CURRENT SOLUTION AFTER GLOBAL IMPROVEMENT
MAX_LOCAL_SEARCH_SIZE = 30000
#############################################################################

 mutable struct _Solution

      maintenancePositions::Array{Int32, 2}
                                    #2-dimensional array to define the slots in
                                    #each machine where maintenances are done.
                                    #First index indicates the machine

      machineCompletionTimes::Array{Int32, 1}
                                    #Stores the completion time in each machine

      machineScheduling::Array{Int32, 2}
                                    #2-dimensional array to define the activity
                                    #done in each slot in each machine
                                    #First index indicates the machine
                                    #An positive index (> 0) represent the job index.
                                    #Otherwise we have a maintenance, represented by the value -1

     solutionValue::Float64         #Stores solution objective function value

     _Solution()  =new()
end

globalBestSolution =  _Solution();
globalBestSolutionValue = 1e100


function evaluateCompletionTimes(solution, NUMBER_OF_MACHINES, p, d, t)

     for i = 1: NUMBER_OF_MACHINES
          completionTimeOnMachine(solution,machine, p, d, t)
     end
end

function completionTimeOnMachine(solution,machine, p, d, t)
     accumulated = 0
     deterioration = 1
     jobsOnMachine = size(currentSolution.machineScheduling[machine])

     for i in 1:jobsOnMachine
          if solution.machineScheduling[machine, i] == -1
               deterioration = 1
               accumulated += t[machine]
               continue
          end

          accumulated += p[i,machine]*deterioration
          deterioration *= d[i,machine]
     end

     solution.machineCompletionTimes[machine] = accumulated
end

################    Function to test where to put machine maintenances     ####################
#### This function uses the propertie that the optimal solution has no accumulated        #####
####  (between maintenances) deterioration delay bigger than the machine maintenance time #####
function  insertMaintenances(solution, NUMBER_OF_MACHINES, NUMBER_OF_JOBS, p,d,t)

     accumulatedDeterioration = ones(NUMBER_OF_MACHINES)
     accumulatedWork = zeros(NUMBER_OF_MACHINES)
     accumulatedDelay = zeros(NUMBER_OF_MACHINES)

     #### All the machines have their schedules checked from the end to the beggining
     ### Evaluating the delay and deterioration from the current job
     ### until the next maintenance (or the last job) we check if the cumulated
     ### delay is greater than a maintenance. If it is, the maintenance is inserted
     ### one position before and all the delay is removed from the final time
     solution.maintenancePositions = Array{Int32,2}(undef,NUMBER_OF_MACHINES,0)
     for i in 1:NUMBER_OF_MACHINES
          jobsOnMachine = size(solution.machineScheduling[i,:])
          println(jobsOnMachine)
          accumulatedWork[i] = 0
          if jobsOnMachine >1
               for j in (jobsOnMachine-1):1
                    job = solution.machineScheduling[i,j]
                    accumulatedDeterioration[i] *= d[job,i]
                    accumulatedWork[i] += p[job,i]
                    accumulatedDelay[i] += accumulatedWork[i]+accumulatedDeterioration[i]

                    ### Sufficient condition to a maintenance be useful
                    if accumulatedDelay[i] > t[i]
                        accumulatedWork[i] = 0
                        accumulatedDelay[i] = 0
                        accumulatedDeterioration[i] = 1

                        #remove the item "element" of the position "index" insert the values [-1, element] instead
                        splice!(initialSolution.machineScheduling[i,j], j:j, [-1, job] )
                        push!(initialSolution.maintenancePositions[i],i)
                    end
               end
          end
     end
end


function evaluateSolution(solution, machines, p, d, t)

     for i in 1:size(machines)
         completionTimeOnMachine(solution,machines[i], p, d, t)

         if solution.machineCompletionTimes[machines[i]] > solution.solutionValue
              solution.solutionValue= currentSolution.machineCompletionTimes[machines[i]]
         end
     end

     return solution.solutionValue
end

function updateBestGlobalSolution(currentSolution, iterationsWithoutImprovement)
     if currentSolution.solutionValue < globalBestSolutionValue
          globalBestSolution = currentSolution(currentSolution)
          iterationsWithoutImprovement = 0
     end
     return iterationsWithoutImprovement
end


############### Function to set the current/best solution after a swap #######################
#### This function does not make any search on the input solution, just evaluate it  #########
function updateBestSolutionsOnSwapLS(initialSolution, previousSolution, solutionValue, currentSolution, bestSolution, bestSolutionOnMachineLoop)
     #### Evaluate if it is the best solution in the local search
     if solutionValue < bestSolutionValue
        bestSolution.solutionValue = solutionValue
        bestSolution = Solution(currentSolution)
     end

     #### Evaluate if it is the best solution in the local search and current machine
     if SWAP_UPDATE_STRATEGY == 2 && solutionValue < bestSolutionOnMachineLoop.solutionValue
          bestSolutionOnMachineLoop = Solution(currentSolution)
     elseif SWAP_UPDATE_STRATEGY == 2
          currentSolution = Solution(previousSolution)
     end

     ### Current solution update strategy
     if SWAP_UPDATE_STRATEGY == 1        ## Never update the current solution
          currentSolution = previousSolution
     elseif SWAP_UPDATE_STRATEGY == 3   ## Update the current solution if it is the best
                                        ## global solution
          if solutionValue < globalBestSolutionValue
               globalBestSolution = currentSolution
               globalBestSolutionValue = currentSolution.solutionValue
          else
               currentSolution = Solution(initialSolution)
          end
     end
end


##################################### LOCAL SEARCHES AREA ####################################

################################ Function to test swaps between 2 JOBS #######################
#### This function does not do maintenance swaps, but it can include/remove maintenances #####
####      if it is needed to make the machine completion time fall                       #####
function internal2Swap(initialSolution, NUMBER_OF_JOBS, NUMBER_OF_MACHINES, d, p, t)
     bestSolution = Solution(initialSolution)
     currentSolution = Solution(initialSolution)
     triedAll = false
     localSeachSize =0
     machine = 1              #Where the local search is being applied
     firstSwapSlot = 1        #The first slot (in a swap)
     lastSwapSlot = 2         #The second slot (in a swap)
     timeUntilF

     numberOfPeriodsOnMachine = size(currentSolution.machineScheduling(machine),1)

     bestSolutionOnMachineLoop = Solution(currentSolution)
     bestSolutionOnMachineLoopValue = 1e100

     while triedAll == false || localSeachSize < MAX_LOCAL_SEARCH_SIZE

           localSeachSize +=1

           ## All the possible swaps with the "currentFirstSlot" were already done
           if lastSwapSlot > numberOfPeriodsOnMachine
                firstSwapSlot+=1

                ## There is no need to put a "while" loop here because it's not
                ## allowed two consective maintenances without a job in between
                if currentSolution.machineScheduling[machine,firstSwapSlot] == -1
                    firstSwapSlot+=1
                end

                ### All the swaps in the current machine were already tested
                ### so we should go to the next one
                if firstSwapSlot >= numberOfPeriodsOnMachine
                   machine +=1
                   firstSwapSlot =1     # We don't need to verify if it is a maintenance
                                         # once they are not allowed in the first slot
                                         # in a machine
                    if SWAP_UPDATE_STRATEGY == 2
                          currentSolution = Solution(bestSolutionOnMachineLoop)
                          bestSolutionOnMachineLoop = Solution(initialSolution)
                          bestSolutionOnMachineLoopValue = 1e100
                    end
                end

                ### All machines were already tested
                ### so the search can finish
                if machine > NUMBER_OF_MACHINES
                     triedAll == true
                     continue
                end

                lastSwapSlot = firstSwapSlot + 1
           end

           ## If the second slot is a maintenance, we should go to the next slot
           ## To shortness of the code, we use the 'continue' statement to make all
           ## the swap validity verification
           if currentSolution.machineScheduling[machine,lastSwapSlot] == -1
                lastSwapSlot += 1
                continue
           end

           ###  Effectivelly do the swap
           previousSolution = Solution(currentSolution)
           firstJob = currentSolution.machineScheduling[machine,lastSwapSlot]
           currentSolution.machineScheduling[machine,lastSwapSlot] = currentSolution.machineScheduling[machine,firstSwapSlot]
           currentSolution.machineScheduling[machine,firstSwapSlot] = firstJob

           solutionValue = evaluateSolution(currentSolution,[machine],p, d,t)
           updateBestSolutionsOnSwapLS(initialSolution, previousSolution, solutionValue, currentSolution, bestSolution, bestSolutionOnMachineLoop)
           lastSwapSlot = lastSwapSlot+1

     end
end

################################ Function to test swaps between 3 JOBS #######################
#### This function does not do maintenance swaps, but it can include/remove maintenances #####
####      if it is needed to make the machine completion time fall                       #####
function internal3Swap(initialSolution, NUMBER_OF_JOBS, NUMBER_OF_MACHINES, d, p, t)
        bestSolutionValue = 1e100
        currentSolution = initialSolution
        triedAll = false
        localSearchSize =0
        currentMachine = 1
        firstSwapSlot = 1
        secondSwapSlot =2
        thirdSwapSlot = 3

         while triedAll == false || localSearchSize < MAX_LOCAL_SEARCH_SIZE

               localSearchSize +=1

               if thirdSwapSlot > numberOfPeriodsOnMachine
                    secondSwapSlot+=1

                    if currentSolution.machineScheduling[machine,secondSwapSlot] == -1
                       secondSwapSlot+=1
                    end

                    if secondSwapSlot >= numberOfPeriodsOnMachine
                         firstSwapSlot +=1
                         secondSwapSlot = firstSwapSlot+1
                         thirdSwapSlot = secondSwapSlot+1

                         ## There is no need to put a "while" loop here because it's not
                         ## allowed two consective maintenances without a job in between
                         if currentSolution.machineScheduling[machine,firstSwapSlot] == -1
                              firstSwapSlot+=1
                         end

                         ### All the swaps in the current machine were already tested
                         ### so we should go to the next one
                         if firstSwapSlot >= numberOfPeriodsOnMachine-1
                              machine +=1
                              firstSwapSlot =1     # We don't need to verify if it is a maintenance
                                                 # once they are not allowed in the first slot
                                                 # in a machine
                              if SWAP_UPDATE_STRATEGY == 2
                                   currentSolution = Solution(bestSolutionOnMachineLoop)
                                   bestSolutionOnMachineLoop = Solution(initialSolution)
                                   bestSolutionOnMachineLoopValue = 1e100
                             else
                         end

                         ### All machines were already tested
                         ### so the search can finish
                         if machine > NUMBER_OF_MACHINES
                            triedAll == true
                            continue
                         end
                         secondSwapSlot = firstSwapSlot + 1
                         thirdSwapSlot = secondSwapSlot + 1
                    end

                    thirdSwapSlot = secondSwapSlot +1
                 end
             end

             ## If the second slot is a maintenance, we should go to the next slot
             ## To shortness of the code, we use the 'continue' statement to make all
             ## the swap validity verification
             if currentSolution.machineScheduling[machine,thirdSwapSlot] == -1
                  thirdSwapSlot += 1
                  localSearchSize -=1
                  continue
             end

             ###  Effectivelly do the swap
             ### Only two permutations are tested (2 3 1) (3 1 2) because the others are 2-items swaps
             previousSolution = Solution(currentSolution)

             ## First permutation
             firstJob = currentSolution.machineScheduling[machine,secondSwapSlot]
             currentSolution.machineScheduling[machine,secondSwapSlot] = currentSolution.machineScheduling[machine,firstSwapSlot]
             currentSolution.machineScheduling[machine,firstSwapSlot] = firstJob
             firstJob = currentSolution.machineScheduling[machine,thirdSwapSlot]
             currentSolution.machineScheduling[machine,thirdSwapSlot] = currentSolution.machineScheduling[machine,secondSwapSlot]
             currentSolution.machineScheduling[machine,secondSwapSlot] = firstJob

             solutionValue = evaluateSolution(currentSolution,[machine], p,d, t)
             updateBestSolutionsOnSwapLS(initialSolution, previousSolution, solutionValue, currentSolution, bestSolution, bestSolutionOnMachineLoop)

             ### Second permutation
             firstJob = currentSolution.machineScheduling[machine,secondSwapSlot]
             currentSolution.machineScheduling[machine,secondSwapSlot] = currentSolution.machineScheduling[machine,firstSwapSlot]
             currentSolution.machineScheduling[machine,firstSwapSlot] = firstJob

             firstJob = currentSolution.machineScheduling[machine,thirdSwapSlot]
             currentSolution.machineScheduling[machine,thirdSwapSlot] = currentSolution.machineScheduling[machine,firstSwapSlot]
             currentSolution.machineScheduling[machine,firstSwapSlot] = firstJob

             solutionValue = evaluateSolution(currentSolution,[machine], p, d, t)
             updateBestSolutionsOnSwapLS(initialSolution, previousSolution, solutionValue, currentSolution, bestSolution, bestSolutionOnMachineLoop)


             thirdSwapSlot+=1
       end
end


########## Function to test swaps between 2 jobs in DIFFERENT machines #######################
#### This function does not do maintenance swaps, but it can include/remove maintenances #####
####      if it is needed to make the machine completion time fall                       #####
function external2Swap(initialSolution, NUMBER_OF_JOBS, NUMBER_OF_MACHINES, d, p, t)

    bestSolutionValue = 1e100
    currentSolution = initialSolution
    triedAll = false
    localSearchSize =0
    firstMachine = 1
    secondMachine = 2
    firstSwapSlot = 1
    secondSwapSlot =1

     while triedAll == false || localSearchSize < MAX_LOCAL_SEARCH_SIZE

           localSearchSize +=1

           ### Check if the position is valid in the first machine
           ### if it's not, the machines are changed
           if firstSwapSlot >  size(currentSolution.machineScheduling[firstMachine])
               firstSwapSlot = 1
               secondSwapSlot = 1
               firstMachine = firstMachine+1
               secondMachine = secondMachine+1
           end

           ### Check if the position is valid in the second machine
           ### if it's not, the second machine is changed
           if secondSwapSlot >  size(currentSolution.machineScheduling[secondMachine])
               firstSwapSlot = 1
               secondSwapSlot = 1
               secondMachine = secondMachine+1
           end

           ### Verifies if all the machines were already tested
           if firstMachine > NUMBER_OF_MACHINES || secondMachine > NUMBER_OF_MACHINES
               triedAll = true
               continue
           end

           ### Verifies if the first slot have a mantainance procedure
           ### if it is true, the loop goes back and the previous validations are re-done
           if currentSolution.machineScheduling[firstMachine,firstSwapSlot] == -1
               firstSwapSlot += 1
               localSearchSize -= 1
               continue
           end

           ### Verifies if the second slot have a mantainance procedure
           ### if it is true, the loop goes back and the previous validations are re-done
           if currentSolution.machineScheduling[secondMachine,secondSwapSlot] == -1
               secondSwapSlot +=1
               localSearchSize -=1
               continue
           end

           #This part is where actually is done the swap of the jobs. After
           previousSolution = Solution(currentSolution)
           firstJob = currentSolution.machineScheduling[secondMachine, secondSwapSlot]
           currentSolution.machineScheduling[secondMachine,secondMachineSlot] = currentSolution.machineScheduling[firstMachine,firstMachine]
           currentSolution.machineScheduling[firstMachine, firstSwapSlot] = firstJob

           if bestSolution.solutionValue > currentSolution.lastSwapSlot
               bestSolution = Solution(currentSolution)
           end

           currentSolution = Solution(previousSolution)
     end
end


########## Function to test swaps between 3 jobs in DIFFERENT machines #######################
#### This function does not do maintenance swaps, but it can include/remove maintenances #####
####      if it is needed to make the machine completion time fall                       #####
function external3Swap(initialSolution, NUMBER_OF_JOBS, NUMBER_OF_MACHINES, d, p, t)
    bestSolutionValue = 1e100
    currentSolution = initialSolution
    triedAll = false
    localSearchSize =0
    firstMachine = 1
    secondMachine = 2
    thirdMachine = 3
    firstSwapSlot = 1
    secondSwapSlot =1
    thirdMachine = 1

     while triedAll == false || localSearchSize < MAX_LOCAL_SEARCH_SIZE

           localSearchSize +=1

           ### Check if the position is valid in the first machine
           ### if it's not, the machines are changed
           if firstSwapSlot >  size(currentSolution.machineScheduling[firstMachine])
               firstSwapSlot = 1
               secondSwapSlot = 1
               firstMachine = firstMachine+1
               secondMachine = secondMachine+1
           end

           ### Check if the position is valid in the second machine
           ### if it's not, the second machine is changed
           if secondSwapSlot >  size(currentSolution.machineScheduling[secondMachine])
               firstSwapSlot = 1
               secondSwapSlot = 1
               thirdSwapSlot = 1
               secondMachine +=1
               thirdMachine += 1
           end

           ### Check if the position is valid in the second machine
           ### if it's not, the second machine is changed
           if thirdSwapSlot >  size(currentSolution.machineScheduling[thirdMachine])
               firstSwapSlot = 1
               secondSwapSlot = 1
               thirdSwapSlot = 1
               thirdMachine +=1
           end

           ### Verifies if all the machines were already tested
           if firstMachine > NUMBER_OF_MACHINES || secondMachine > NUMBER_OF_MACHINES || thirdMachine > NUMBER_OF_MACHINES
               triedAll = true
               continue
           end

           ### Verifies if the first slot have a mantainance procedure
           ### if it is true, the loop goes back and the previous validations are re-done
           if currentSolution.machineScheduling[firstMachine,firstSwapSlot] == -1
               firstSwapSlot += 1
               localSearchSize -= 1
               continue
           end

           ### Verifies if the second slot have a mantainance procedure
           ### if it is true, the loop goes back and the previous validations are re-done
           if currentSolution.machineScheduling[secondMachine,secondSwapSlot] == -1
               secondSwapSlot +=1
               localSearchSize -= 1
               continue
           end

           ### Verifies if the second slot have a mantainance procedure
           ### if it is true, the loop goes back and the previous validations are re-done
           if currentSolution.machineScheduling[thirdMachine,thirdSwapSlot] == -1
               thirdSwapSlot +=1
               localSearchSize -= 1
               continue
           end

           previousSolution = Solution(currentSolution)
           firstJob = currentSolution.machineScheduling[secondMachine, secondSwapSlot]
           currentSolution.machineScheduling[secondMachine,secondMachineSlot] =  currentSolution.machineScheduling[firstMachine, firstSwapSlot]
           currentSolution.machineScheduling[firstMachine, firstSwapSlot] = firstJob

           firstJob = currentSolution.machineScheduling[thirdMachine, thirdSwapSlot]
           currentSolution.machineScheduling[thirdMachine,thirdMachineSlot] = currentSolution.machineScheduling[secondMachine, secondSwapSlot]
           currentSolution.machineScheduling[secondMachine, secondSwapSlot] = firstJob

           if bestSolution.solutionValue > currentSolution.lastSwapSlot
               bestSolution = Solution(currentSolution)
           end

           currentSolution = Solution(previousSolution)

           firstJob = currentSolution.machineScheduling[secondMachine, secondSwapSlot]
           currentSolution.machineScheduling[secondMachine,secondMachineSlot] =  currentSolution.machineScheduling[firstMachine, firstSwapSlot]
           currentSolution.machineScheduling[firstMachine, firstSwapSlot] = firstJob

           firstJob = currentSolution.machineScheduling[thirdMachine, thirdSwapSlot]
           currentSolution.machineScheduling[thirdMachine,thirdMachineSlot] =  currentSolution.machineScheduling[firstMachine, firstSwapSlot]
           currentSolution.machineScheduling[firstMachine, firstSwapSlot] = firstJob

           if bestSolution.solutionValue > currentSolution.lastSwapSlot
              bestSolution = Solution(currentSolution)
           end

           currentSolution = Solution(previousSolution)
     end
end


function runTaskBalacing(initialSolution, NUMBER_OF_JOBS, NUMBER_OF_MACHINES, d, p, t)

     currentSolution = Solution(initialSolution)
     bestSolution = Solution(currentSolution)
     firstRun = zeros(NUMBER_OF_MACHINES)

     #while

     improved = true
     while(improved)
          improved = true

     end
end

#################################### INITIAL SOLUTIONS STRATEGIES ###############################

############### Function to build a initial solution based on processing time   ##################
######## This function takes the jobs with bigger processing times and put them first ############
######## in the machine. It can permit lower delays, as bigger times are in begining  ############
##################################################################################################
function biggerTasksFirst(NUMBER_OF_JOBS, NUMBER_OF_MACHINES, p, d, t)
     jobOrderedByDuration = Array{Pair{Float64, Int32},2}(undef,NUMBER_OF_MACHINES,NUMBER_OF_JOBS)
     solution = _Solution()


     for i in 1:NUMBER_OF_MACHINES
          for j in 1:NUMBER_OF_JOBS
               jobOrderedByDuration[i,j] = Pair{Float64, Int32}(p[j,i],j)
          end

          sort(jobOrderedByDuration[i,:], by = first, rev=true)
     end

     usedJobs = zeros(NUMBER_OF_JOBS)
     usedMachines = zeros(NUMBER_OF_MACHINES)
     countInserted = 0
     solution.machineScheduling = Array{Int32, 2}(undef,NUMBER_OF_MACHINES, 0)
     for i in 1:NUMBER_OF_MACHINES
          for j in 1:NUMBER_OF_JOBS
               if usedJobs[jobOrderedByDuration[i,j].second] == 0

                  usedJobs[jobOrderedByDuration[i,j].second] =1
                  if usedMachines[i] == 0
                    println("HERE")
                    resize!(solution.machineScheduling[i,:],1)

                    append!(solution.machineScheduling[i,:], jobOrderedByDuration[i,j].second)
                    println("****", solution.machineScheduling[i,:])
                  else
                    pushfirst!(solution.machineScheduling[i,:],jobOrderedByDuration[i,j].second)
                  end

                  usedMachines[i] = 1

                  println(solution.machineScheduling[i,:])
                  countInserted +=1
               end
          end
          if countInserted == NUMBER_OF_JOBS
               break
          end
     end
     ### In this point will be inserted the maintenances

     insertMaintenances(solution, NUMBER_OF_MACHINES, NUMBER_OF_JOBS, p,d,t)


     evaluateCompletionTimes(solution, NUMBER_OF_MACHINES, p, d, t)

     return solution
end


############### Function to build a initial solution based on expected delay   ##################
######## This function takes the jobs with lower deteriorations and put them first   ############
######## in the machine. It can permit more tasks be done without a maintenance      ############
#################################################################################################
function lowerExpectDelayFirst(NUMBER_OF_JOBS, NUMBER_OF_MACHINES, d, p, t)
     jobOrderedByDeterioration = Array{Pair{Float64, Int32},2}(undef,NUMBER_OF_MACHINES,NUMBER_OF_JOBS)
     solution = Solution(zeros(0,0), zeros(0,0), zeros(0), zeros(0,0), -1)

     for i in 1:NUMBER_OF_MACHINES
          for j in 1:NUMBER_OF_JOBS
               jobOrderedByDeterioration[i,j] = p[j,i]
          end
          sort(jobOrderedByDeterioration[i])
     end

     jobUsed = zeros(NUMBER_OF_JOBS)
     countInserted = 0
     for i in 1:NUMBER_OF_MACHINES
          for j in 1:NUMBER_OF_JOBS
               if jobUsed[jobOrderedByDeterioration[i,j].second] == 0
                  jobUsed[jobOrderedByDeterioration[i,j].second] =1
                  pushfirst!(solution.machineScheduling[i],jobOrderedByDeterioration[i,j].second)
                  countInserted +=1
               end
          end
          if countInserted == NUMBER_OF_JOBS
               break
          end
     end

     ### In this point will be inserted the maintenances
     insertMaintenances(solution, NUMBER_OF_MACHINES, NUMBER_OF_JOBS, p,d,t)
     evaluateCompletionTimes(solution, NUMBER_OF_MACHINES, p, d, t)

     return solution
end

####################### Function to run a constructive heuristic   ##############################
######## This function takes the jobs with lower deteriorations and put them first   ############
######## in the machine. It can permit more tasks be done without a maintenance      ############
#################################################################################################
function constructiveHeuristic(NUMBER_OF_JOBS, NUMBER_OF_MACHINES, d, p,t)

     if CONSTRUCTIVE_HEURISTIC_STRATEGY == 1
         solution =  biggerTasksFirst(NUMBER_OF_JOBS, NUMBER_OF_MACHINES, d, p, t)
     elseif CONSTRUCTIVE_HEURISTIC_STRATEGY == 2
         solutiopn =  lowerExpectDelayFirst(NUMBER_OF_JOBS, NUMBER_OF_MACHINES, d, p, t)
     end

     return solution
end

################################## PEERTUBATIONS ###############################################
function shiftMachineSchedulings(solution, NUMBER_OF_JOBS, NUMBER_OF_MACHINES, d, p, t)

     if NUMBER_OF_MACHINES> 2
          lastScheduling = solution.machineScheduling[NUMBER_OF_MACHINES]

          for i = NUMBER_OF_MACHINES: 2
               solution.machineScheduling[i] = solution.machineScheduling[i-1]
          end
          solution.machineScheduling[1] = lastScheduling
     end

     evaluateSolution(solution,collect(1:NUMBER_OF_MACHINES), p,d,t)
     return shiftMachineSchedulings
end

##################### Function reverse task job order on machines   ############################
######## This function reverse the job orders in all the machines, recalculating all ###########
######## the maintenances and their respective intervals times                       ###########
################################################################################################
function reverseTasks(initialSolution, NUMBER_OF_JOBS, NUMBER_OF_MACHINES, d, p, t)

     for machine in 1:NUMBER_OF_MACHINES
          completionTime = 0
          deterioration = 1
          initialSolution.machineScheduling[machine] = reverse(initialSolution.machineScheduling[machine])
          job = 1
          while index <= size(initialSolution.machineScheduling[machine])
               job= initialSolution.machineScheduling[machine,index]
               if job >= 0
                    completionTime += p[job, machine]
                    deterioration *= d[job,machine]
                    if job < size(initialSolution.machineScheduling[machine]) && (deterioration-1.0)*d[job,machine] >= t[machine]
                         completionTime += t[machine]
                         deterioration = 1
                         element = initialSolution.machineScheduling[job,machine]
                         #remove the item "element" of the position "index" insert the values [-1, element] instead
                         splice!(initialSolution.machineScheduling[job,machine], index:index, [-1, element] )
                    end
                    index+=1
               else
                    delete(initialSolution.machineScheduling[machine], index)
               end
          end
     end

     return initialSolution
end


function pertubation(initialSolution,  NUMBER_OF_JOBS, NUMBER_OF_MACHINES, p,d, t)



end

function printSolution(initialSolution, NUMBER_OF_MACHINES)

     for i in 1:NUMBER_OF_MACHINES
          println("Scheduling in the machine: ",i)
          for j in 1:size(initialSolution.machineScheduling[i])
               print(initialSolution.machineScheduling[i,j]," " )
          end
     end
end

function mainHeuristic(NUMBER_OF_JOBS, NUMBER_OF_MACHINES, p,d, t)

           iterationsWithoutImprovement = 0

           initialSolution = constructiveHeuristic(NUMBER_OF_JOBS, NUMBER_OF_MACHINES,p,d,t)

           printSolution(initialSolution, NUMBER_OF_MACHINES)


           currentSolution = Solution(initialSolution)
           while iterationsWithoutImprovement < MAX_ITERATIONS_WITHOUT_IMPROVEMENT
                 ### I THINK THIS IF WILL BE NOT NECESSARY
                 if totalTime > MAX_EXECUTION_TIME
                      break
                 end

                 currentSolution = internal2Swap(currentSolution, NUMBER_OF_JOBS, NUMBER_OF_MACHINES, d, p, t)
                 iterationsWithoutImprovement = updateBestGlobalSolution(currentSolution, iterationsWithoutImprovement)

                 currentSolution = internal3Swap(currentSolution, NUMBER_OF_JOBS, NUMBER_OF_MACHINES, d, p, t)
                 iterationsWithoutImprovement = updateBestGlobalSolution(currentSolution, iterationsWithoutImprovement)

                 currentSolution = external2Swap(currentSolution, NUMBER_OF_JOBS, NUMBER_OF_MACHINES, d, p, t)
                 iterationsWithoutImprovement = updateBestGlobalSolution(currentSolution, iterationsWithoutImprovement)

                 currentSolution = external3Swap(currentSolution, NUMBER_OF_JOBS, NUMBER_OF_MACHINES, d, p, t)
                 iterationsWithoutImprovement = updateBestGlobalSolution(currentSolution, iterationsWithoutImprovement)

              ##   currentSolution = runTaskBalacing(currentSolution, NUMBER_OF_JOBS, NUMBER_OF_MACHINES, d, p, t)
              ##   iterationsWithoutImprovement = updateBestGlobalSolution(currentSolution, iterationsWithoutImprovement)

                  if iterationsWithoutImprovement%10 == 9
                      pertubation(initialSolution,  NUMBER_OF_JOBS, NUMBER_OF_MACHINES, p,d, t)
                  end

                 iterationsWithoutImprovement +=1
           end
end

########################################################################################
######                        MAIN FUNCTION                                      #######
########################################################################################
open("Instances/Instances With Deterioration/instance_list.txt") do file
    for instanceName in eachline(file)
        NUMBER_OF_JOBS = 0
        NUMBER_OF_MACHINES = 0
        NUMBER_OF_PERIODS = 0
        open(string("Instances/Instances With Deterioration/Instances/",instanceName)) do instance
           NUMBER_OF_MACHINES = parse(Int64, readline(instance))
           NUMBER_OF_JOBS = parse(Int64, readline(instance))
           NUMBER_OF_PERIODS = 2*(NUMBER_OF_JOBS-NUMBER_OF_MACHINES+1)+1

           if NUMBER_OF_PERIODS <= 0
              NUMBER_OF_PERIODS = 1
           end

           d = zeros(NUMBER_OF_JOBS, NUMBER_OF_MACHINES)
           p = zeros(NUMBER_OF_JOBS, NUMBER_OF_MACHINES)
           t = zeros(NUMBER_OF_MACHINES)

           jobDurationData = readline(instance)
           jobDurations = split(jobDurationData, " ")

           for i in 1:NUMBER_OF_JOBS
               for j in 1:NUMBER_OF_MACHINES
                   p[i,j] = parse(Float64, jobDurations[i])
               end
           end

           maintenanceDurationData = readline(instance)
           maintenanceDuration = split(maintenanceDurationData, " ")

           for i in NUMBER_OF_MACHINES
               t[i] = parse(Float64, maintenanceDuration[i])
           end


           for i in 1:NUMBER_OF_JOBS
                 stringData = readline(instance)
                 rowData = split(stringData, " ")
                 for j in 1:NUMBER_OF_MACHINES
                     d[i,j] = parse(Float64, rowData[j])
                end
           end

           mainHeuristic(NUMBER_OF_JOBS, NUMBER_OF_MACHINES,p,d,t)
        end
    end
end
