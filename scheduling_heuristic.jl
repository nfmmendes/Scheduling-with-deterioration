using JuMP
using CPLEX


#####################      HEURISTIC PARAMETERS     #######################

###############################################################################
### THESE ARE THE DEFAULT VALUES. THEY CAN BE CHANGED IN ANY PART OF THE CODE
###############################################################################
MAX_ITERATIONS_WITHOUT_IMPROVEMENT = 50
MAX_EXECUTION_TIME = 60*30
CONSTRUCTIVE_HEURISTIC_STRATEGY = 1
PERTURBATION_STRATEGY = 1
SWAP_UPDATE_STRATEGY = 1  # 1 = ALWAYS THE SAME CURRENT SOLUTION
                          # 2 = UPDATE CURRENT SOLUTION IN EACH MACHINE
                          # 3 = UPDATE CURRENT SOLUTION AFTER GLOBAL IMPROVEMENT
MAX_LOCAL_SEARCH_SIZE = 30000
#############################################################################


globalBestSolutionValue = 1e100

 mutable struct Solution

      manteinancePositions::Array{Int32, 2}
                                    #2-dimensional array to define the slots in
                                    #each machine where maintenances are done.
                                    #First index indicates the machine

      machineCompletionTimes::Array{Float64, 1}
                                    #Stores the completion time in each machine
      slotsOnMachine::Array{Int32,1}
                                    #Stores how many slots are used on machine
      maintenancesOnMachine::Array{Int32,1}
                                    #Stores how many maintenances are done in the machine
      machineScheduling::Array{Int32, 2}
                                    #2-dimensional array to define the activity
                                    #done in each slot in each machine
                                    #First index indicates the machine
                                    #An positive index (> 0) represent the job index.
                                    #Otherwise we have a maintenance, represented by the value -1

     solutionValue::Float64         #Stores solution objective function value

     Solution()  =new()
     Solution(other::Solution) = new(deepcopy(other.manteinancePositions),
                                     deepcopy(other.machineCompletionTimes),
                                     deepcopy(other.slotsOnMachine),
                                     deepcopy(other.maintenancesOnMachine),
                                     deepcopy(other.machineScheduling),
                                     deepcopy(other.solutionValue))
end

globalBestSolution =  Solution()


function evaluateCompletionTimes!(solution, NUMBER_OF_MACHINES, p, d, t)

     for i = 1: NUMBER_OF_MACHINES
         solution = completionTimeOnMachine!(solution,i, p, d, t)
     end
     return solution
end

#############    Function to calculate the completion time of one machine     ###############
####           This function does not change the scheduling in any way                  #####
function completionTimeOnMachine!(solution,machine, p, d, t)
     accumulated = 0
     deterioration = 1.0
     slotsOnMachine = solution.slotsOnMachine[machine]
    # println("Completion time on machine: ", machine)
    # println("machine scheduling")
    #  println(solution.machineScheduling[machine,:])

     for i in 1:slotsOnMachine
          if solution.machineScheduling[machine, i] == -1
               deterioration = 1
          #     println("Accumulated: ", accumulated, "\t Maintenance:" , maintenance)
               accumulated += t[machine]
               continue
          end

          

          accumulated += p[solution.machineScheduling[machine, i],machine]*deterioration
          deterioration *= d[solution.machineScheduling[machine, i],machine]
     end

     solution.machineCompletionTimes[machine] = accumulated
     return solution
end

###############    Function to insert a operation (job or maintenance) in a machine ###########
### This function does not do any evaluation or validation of the operation inserted        ###
function insertOperation!(machineScheduling,slotsOnMachine,machine, position,operation )
  #   println(machineScheduling)
     if  position < slotsOnMachine && slotsOnMachine < size(machineScheduling[machine,:])[1]
          for i in slotsOnMachine:-1:position
               machineScheduling[machine,i+1] = machineScheduling[machine,i]
          end
          machineScheduling[machine,position] = operation
     end
 #    println(machineScheduling)
     return machineScheduling
end


function removeOperationAt!(machineScheduling, index)
     numberOfSlots = size(machineScheduling)[1]
     if index > 0
          for i in index:numberOfSlots-1
               machineScheduling[i] = machineScheduling[i+1]
          end
     end
     return machineScheduling
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

     for i in 1:NUMBER_OF_MACHINES
          slotsOnMachine = solution.slotsOnMachine[i]

          for j in 1:slotsOnMachine
                if solution.machineScheduling[i,j] == -1
                    slotsOnMachine = slotsOnMachine-1
                    solution.machineScheduling[i,:] = removeOperationAt!(solution.machineScheduling[i,:], j)
                end
          end


          accumulatedWork[i] = 0
          numberOfMaintenances = 0
          if slotsOnMachine >1
               for j in (slotsOnMachine-1):-1:1
                    job = solution.machineScheduling[i,j]

                    accumulatedDeterioration[i] *= d[job,i]
                    accumulatedWork[i] += p[job,i]
                    accumulatedDelay[i] += accumulatedWork[i]+accumulatedDeterioration[i]

                    ### Sufficient condition to a maintenance be useful
                    if accumulatedDelay[i] > t[i] && j > 1
                        accumulatedWork[i] = 0
                        accumulatedDelay[i] = 0
                        accumulatedDeterioration[i] = 1


                        numberOfMaintenances +=1
                        solution.machineScheduling =insertOperation!(solution.machineScheduling,slotsOnMachine,i, j, -1)
                        solution.manteinancePositions[i,numberOfMaintenances]=j

                        slotsOnMachine += 1
                    end
               end
          end

          solution.slotsOnMachine[i] = slotsOnMachine
          solution.maintenancesOnMachine[i] = numberOfMaintenances
     end
     return solution
end


function evaluateSolution!(solution, machines, p, d, t)

     otherSolution = Solution(solution)
     for i in 1:size(machines)[1]
         otherSolution = completionTimeOnMachine!(solution,machines[i], p, d, t)
     end

     makespan = -1

     for i in 1:size(solution.machineCompletionTimes)[1]
        if makespan < solution.machineCompletionTimes[i]
            makespan = solution.machineCompletionTimes[i]
        end
     end

     otherSolution.solutionValue = makespan

     return solution.solutionValue
end

function updateBestGlobalSolution!(currentSolution, iterationsWithoutImprovement)
     global globalBestSolutionValue
     global globalBestSolution
     if currentSolution.solutionValue < globalBestSolutionValue
          print("Best solution found: ", currentSolution.solutionValue, " \tPrevious",globalBestSolutionValue )
          globalBestSolution = Solution(currentSolution)
          global globalBestSolutionValue = currentSolution.solutionValue
          iterationsWithoutImprovement = 0
     end
     return iterationsWithoutImprovement
end


############### Function to set the current/best solution after a swap #######################
#### This function does not make any search on the input solution, just evaluate it  #########
function updateBestSolutionsOnSwapLS!(initialSolution, previousSolution, solutionValue, currentSolution, bestSolution, bestSolutionOnMachineLoop)
     #### Evaluate if it is the best solution in the local search

     global globalBestSolutionValue
     global globalBestSolution
     #println(solutionValue, " ", bestSolution.solutionValue)

     if solutionValue <  bestSolution.solutionValue
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
          currentSolution = Solution(previousSolution)
     elseif SWAP_UPDATE_STRATEGY == 3   ## Update the current solution if it is the best
                                        ## global solution
          if solutionValue < globalBestSolutionValue
               globalBestSolution = Solution(currentSolution)
               globalBestSolutionValue = currentSolution.solutionValue
          else
               currentSolution = Solution(initialSolution)
          end
     end

     return Solution(currentSolution),Solution(bestSolution), bestSolution.solutionValue
end


##################################### LOCAL SEARCHES AREA ####################################

################################ Function to test swaps between 2 JOBS #######################
#### This function does not do maintenance swaps, but it can include/remove maintenances #####
####      if it is needed to make the machine completion time fall                       #####
function internal2Swap(initialSolution, NUMBER_OF_JOBS, NUMBER_OF_MACHINES, d, p, t)
     bestSolution = Solution(initialSolution)
     bestSolution.solutionValue = initialSolution.solutionValue
     currentSolution = Solution(initialSolution)
     triedAll = false
     localSeachSize =0
     machine = 1              #Where the local search is being applied
     firstSwapSlot = 1        #The first slot (in a swap)
     lastSwapSlot = 2         #The second slot (in a swap)


     numberOfSlots = currentSolution.slotsOnMachine[machine]

     bestSolutionOnMachineLoop = Solution(currentSolution)
     bestSolutionOnMachineLoopValue = 1e100

     while triedAll == false && localSeachSize < MAX_LOCAL_SEARCH_SIZE

           localSeachSize +=1

           ### All machines were already tested
           ### so the search can finish
           if machine > NUMBER_OF_MACHINES
                triedAll == true
                continue
           end

           ## All the possible swaps with the "currentFirstSlot" were already done
           if lastSwapSlot > numberOfSlots
                firstSwapSlot+=1

                ## There is no need to put a "while" loop here because it's not
                ## allowed two consective maintenances without a job in between
                if currentSolution.machineScheduling[machine,firstSwapSlot] == -1
                    firstSwapSlot+=1
                end

                ### All the swaps in the current machine were already tested
                ### so we should go to the next one
                if firstSwapSlot >= numberOfSlots
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

                lastSwapSlot = firstSwapSlot + 1
           end

           ### All machines were already tested
           ### so the search can finish
           if machine > NUMBER_OF_MACHINES
                triedAll == true
                continue
           end

           ## If the second slot is a maintenance, we should go to the next slot
           ## To shortness of the code, we use the 'continue' statement to make all
           ## the swap validity verification
           if currentSolution.machineScheduling[machine,lastSwapSlot] == -1
                lastSwapSlot += 1
                continue
           end

        # println("Scheduling before 2 internal swap")
        #  println(currentSolution.machineScheduling[machine,:])
           ###  Effectivelly do the swap
           previousSolution = Solution(currentSolution)
           firstJob = currentSolution.machineScheduling[machine,lastSwapSlot]
           currentSolution.machineScheduling[machine,lastSwapSlot] = currentSolution.machineScheduling[machine,firstSwapSlot]
           currentSolution.machineScheduling[machine,firstSwapSlot] = firstJob


           currentSolution = insertMaintenances(currentSolution, NUMBER_OF_MACHINES, NUMBER_OF_JOBS, p,d,t)

           solutionValue = evaluateSolution!(currentSolution,[machine],p, d,t)
           currentSolution.solutionValue = solutionValue




           currentSolution,bestSolution, bestSolutionValue =
                          updateBestSolutionsOnSwapLS!(initialSolution, previousSolution,
                                                       solutionValue, currentSolution, bestSolution,
                                                       bestSolutionOnMachineLoop)
            printSolution(bestSolution, NUMBER_OF_MACHINES)
         # println("Scheduling after solution evaluation on  internal swap")
         # println(currentSolution.machineScheduling[machine,:])
           lastSwapSlot = lastSwapSlot+1


     end


     return bestSolution

end

################################ Function to test swaps between 3 JOBS #######################
#### This function does not do maintenance swaps, but it can include/remove maintenances #####
####      if it is needed to make the machine completion time fall                       #####
function internal3Swap(initialSolution, NUMBER_OF_JOBS, NUMBER_OF_MACHINES, d, p, t)
        bestSolution = Solution()
        bestSolutionValue = 1e100
        bestSolution.solutionValue = 1e100
        bestSolutionOnMachineLoopValue = 1e100
        currentSolution =  Solution(initialSolution)
        bestSolutionOnMachineLoop = Solution(currentSolution)
        triedAll = false
        localSearchSize =0
        machine = 1
        firstSwapSlot = 1
        secondSwapSlot =2
        thirdSwapSlot = 3

        numberOfSlots = currentSolution.slotsOnMachine

        while triedAll == false && localSearchSize < MAX_LOCAL_SEARCH_SIZE

               localSearchSize +=1

               ### All machines were already tested
               ### so the search can finish
               if machine > NUMBER_OF_MACHINES
                  triedAll == true
                  continue
               end

               if thirdSwapSlot > numberOfSlots[machine]
                    secondSwapSlot+=1

                    if currentSolution.machineScheduling[machine,secondSwapSlot] == -1
                       secondSwapSlot+=1
                    end

                    if secondSwapSlot >= numberOfSlots[machine]
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
                         if firstSwapSlot >= numberOfSlots[machine]-1
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

                         secondSwapSlot = firstSwapSlot + 1
                         thirdSwapSlot = secondSwapSlot + 1
                    end

                    thirdSwapSlot = secondSwapSlot +1
                 end
             end

             ### All machines were already tested
             ### so the search can finish
             if machine > NUMBER_OF_MACHINES
               triedAll == true
               continue
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

            # println("Scheduling before 3 internal swap")
           #  println(currentSolution.machineScheduling[machine,:])
             ## First permutation
             firstJob = currentSolution.machineScheduling[machine,secondSwapSlot]
             currentSolution.machineScheduling[machine,secondSwapSlot] = currentSolution.machineScheduling[machine,firstSwapSlot]
             currentSolution.machineScheduling[machine,firstSwapSlot] = firstJob
             firstJob = currentSolution.machineScheduling[machine,thirdSwapSlot]
             currentSolution.machineScheduling[machine,thirdSwapSlot] = currentSolution.machineScheduling[machine,secondSwapSlot]
             currentSolution.machineScheduling[machine,secondSwapSlot] = firstJob
            # println("Scheduling before 3 internal swap")
            # println(currentSolution.machineScheduling[machine,:])
            # sleep(1)

             solutionValue = evaluateSolution!(currentSolution,[machine], p,d, t)
             currentSolution.solutionValue = solutionValue
             currentSolution, bestSolution, bestSolutionValue =
                              updateBestSolutionsOnSwapLS!(initialSolution, previousSolution,
                                                           solutionValue, currentSolution,
                                                           bestSolution, bestSolutionOnMachineLoop)

             ### Second permutation
             firstJob = currentSolution.machineScheduling[machine,secondSwapSlot]
             currentSolution.machineScheduling[machine,secondSwapSlot] = currentSolution.machineScheduling[machine,firstSwapSlot]
             currentSolution.machineScheduling[machine,firstSwapSlot] = firstJob

             firstJob = currentSolution.machineScheduling[machine,thirdSwapSlot]
             currentSolution.machineScheduling[machine,thirdSwapSlot] = currentSolution.machineScheduling[machine,firstSwapSlot]
             currentSolution.machineScheduling[machine,firstSwapSlot] = firstJob

             solutionValue = evaluateSolution!(currentSolution,[machine], p, d, t)
             currentSolution.solutionValue = solutionValue
             currentSolution, bestSolution, bestSolutionValue =
                           updateBestSolutionsOnSwapLS!(initialSolution, previousSolution,
                                                        solutionValue, currentSolution,
                                                        bestSolution, bestSolutionOnMachineLoop)

             thirdSwapSlot+=1
       end
       return bestSolution
end


########## Function to test swaps between 2 jobs in DIFFERENT machines #######################
#### This function does not do maintenance swaps, but it can include/remove maintenances #####
####      if it is needed to make the machine completion time fall                       #####
function external2Swap(initialSolution, NUMBER_OF_JOBS, NUMBER_OF_MACHINES, d, p, t)
    bestSolution = Solution()
    bestSolution.solutionValue = 1e100
    bestSolutionValue = 1e100
    currentSolution = Solution(initialSolution)
    triedAll = false
    localSearchSize =0
    firstMachine = 1
    secondMachine = 2
    firstSwapSlot = 1
    secondSwapSlot =1

    firstMachineNumberOfSlots = currentSolution.slotsOnMachine[firstMachine]
    secondMachineNumberOfSlots = currentSolution.slotsOnMachine[secondMachine]

    while triedAll == false && localSearchSize < MAX_LOCAL_SEARCH_SIZE

        localSearchSize +=1


        ### Check if the position is valid in the first machine
        ### if it's not, the machines are changed
        if firstSwapSlot >  firstMachineNumberOfSlots
            firstSwapSlot = 1
            secondSwapSlot = 1
            firstMachine = firstMachine+1
            secondMachine = secondMachine+1
        end

        ### Check if the position is valid in the second machine
        ### if it's not, the second machine is changed
        if secondSwapSlot >  secondMachineNumberOfSlots
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
        currentSolution.machineScheduling[secondMachine,secondSwapSlot] = currentSolution.machineScheduling[firstMachine,firstMachine]
        currentSolution.machineScheduling[firstMachine, firstSwapSlot] = firstJob

        solutionValue = evaluateSolution!(currentSolution,[firstMachine,secondMachine], p, d, t)
        currentSolution.solutionValue = solutionValue

        if bestSolutionValue > currentSolution.solutionValue
            bestSolution = Solution(currentSolution)
            bestSolutionValue = currentSolution.solutionValue
        end

        currentSolution = Solution(previousSolution)
     end
     return bestSolution
end


########## Function to test swaps between 3 jobs in DIFFERENT machines #######################
#### This function does not do maintenance swaps, but it can include/remove maintenances #####
####      if it is needed to make the machine completion time fall                       #####
function external3Swap(initialSolution, NUMBER_OF_JOBS, NUMBER_OF_MACHINES, d, p, t)

    bestSolution = Solution()
    bestSolution.solutionValue = 1e100
    bestSolutionValue = 1e100
    currentSolution = Solution(initialSolution)
    triedAll = false
    localSearchSize =0
    firstMachine = 1
    secondMachine = 2
    thirdMachine = 3
    firstSwapSlot = 1
    secondSwapSlot =1
    thirdSwapSlot = 1

    if NUMBER_OF_MACHINES < 3
        return initialSolution
    end

    firstMachineNumberOfSlots = currentSolution.slotsOnMachine[firstMachine]
    secondMachineNumberOfSlots = currentSolution.slotsOnMachine[secondMachine]
    thirdMachineNumberOfSlots = currentSolution.slotsOnMachine[thirdMachine]

    while triedAll == false && localSearchSize < MAX_LOCAL_SEARCH_SIZE

           localSearchSize +=1

           ### Check if the position is valid in the first machine
           ### if it's not, the machines are changed
           if firstSwapSlot >  firstMachineNumberOfSlots
               firstSwapSlot = 1
               secondSwapSlot = 1
               firstMachine = firstMachine+1
               secondMachine = secondMachine+1
           end

           ### Check if the position is valid in the second machine
           ### if it's not, the second machine is changed
           if secondSwapSlot >  secondMachineNumberOfSlots
               firstSwapSlot = 1
               secondSwapSlot = 1
               thirdSwapSlot = 1
               secondMachine +=1
               thirdMachine += 1
           end

           ### Check if the position is valid in the second machine
           ### if it's not, the second machine is changed
           if thirdSwapSlot >  thirdMachineNumberOfSlots
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
           currentSolution.machineScheduling[secondMachine,secondSwapSlot] =  currentSolution.machineScheduling[firstMachine, firstSwapSlot]
           currentSolution.machineScheduling[firstMachine, firstSwapSlot] = firstJob

           firstJob = currentSolution.machineScheduling[thirdMachine, thirdSwapSlot]
           currentSolution.machineScheduling[thirdMachine,thirdSwapSlot] = currentSolution.machineScheduling[secondMachine, secondSwapSlot]
           currentSolution.machineScheduling[secondMachine, secondSwapSlot] = firstJob

           if bestSolutionValue > currentSolution.solutionValue
            #   println("3-external swap")
               bestSolution = Solution(currentSolution)
               bestSolutionValue = currentSolution.solutionValue
           end

           currentSolution = Solution(previousSolution)

           firstJob = currentSolution.machineScheduling[secondMachine, secondSwapSlot]
           currentSolution.machineScheduling[secondMachine,secondSwapSlot] =  currentSolution.machineScheduling[firstMachine, firstSwapSlot]
           currentSolution.machineScheduling[firstMachine, firstSwapSlot] = firstJob

           firstJob = currentSolution.machineScheduling[thirdMachine, thirdSwapSlot]
           currentSolution.machineScheduling[thirdMachine,thirdSwapSlot] =  currentSolution.machineScheduling[firstMachine, firstSwapSlot]
           currentSolution.machineScheduling[firstMachine, firstSwapSlot] = firstJob

           if bestSolution.solutionValue > currentSolution.solutionValue
              bestSolution = Solution(currentSolution)
           end

           currentSolution = Solution(previousSolution)
     end
     return bestSolution
end


function runTaskBalacing(initialSolution, NUMBER_OF_JOBS, NUMBER_OF_MACHINES, d, p, t)

     currentSolution = Solution(initialSolution)
     bestSolution = Solution(currentSolution)
     firstRun = zeros(NUMBER_OF_MACHINES)

     #while

     improved = true
     while(improved)
          improved = false


     end
end

#################################### INITIAL SOLUTIONS STRATEGIES ###############################

############### Function to build a initial solution based on processing time   ##################
######## This function takes the jobs with bigger processing times and put them first ############
######## in the machine. It can permit lower delays, as bigger times are in begining  ############
##################################################################################################
function biggerTasksFirst(NUMBER_OF_JOBS, NUMBER_OF_MACHINES, p, d, t)
     jobOrderedByDuration = Array{Pair{Float64, Int32},2}(undef,NUMBER_OF_MACHINES,NUMBER_OF_JOBS)
     solution = Solution()
     solution.slotsOnMachine= zeros(NUMBER_OF_MACHINES)
     solution.maintenancesOnMachine = zeros(NUMBER_OF_MACHINES)
     solution.manteinancePositions = zeros(NUMBER_OF_MACHINES, NUMBER_OF_JOBS)
     solution.machineScheduling = zeros(NUMBER_OF_MACHINES, 2*NUMBER_OF_JOBS)
     solution.machineCompletionTimes = zeros(NUMBER_OF_MACHINES)

     for i in 1:NUMBER_OF_MACHINES
          auxJobsOrdered = Array{Pair{Float64, Int32}}(undef,NUMBER_OF_JOBS)
          for j in 1:NUMBER_OF_JOBS
               auxJobsOrdered[j] = Pair{Float64, Int32}(p[j,i],j)
          end

          sort!(auxJobsOrdered, by = x->x[1], rev=true)
          jobOrderedByDuration[i,:] = auxJobsOrdered
     end

     usedJobs = zeros(NUMBER_OF_JOBS)
     countInserted = 0

     numberOfSlots = Array{Int32}(undef,NUMBER_OF_MACHINES)
     for i in 1:NUMBER_OF_MACHINES
          numberOfSlots[i] = 0
     end

     i = 0
     while true
          i += 1
         # println("____________________________________")
          if i > NUMBER_OF_MACHINES
               i = 1
          end
          for j in 1:NUMBER_OF_JOBS
               if usedJobs[jobOrderedByDuration[i,j].second] == 0
             #    println(jobOrderedByDuration[i,j].second)
                  usedJobs[jobOrderedByDuration[i,j].second] =1
                  numberOfSlots[i] +=1
                  solution.machineScheduling[i,numberOfSlots[i]] =jobOrderedByDuration[i,j].second
             #     println(solution.machineScheduling[i,:])
                  countInserted +=1
                  break
               end
          end
          if countInserted == NUMBER_OF_JOBS
               break
          end

     end

     solution.slotsOnMachine= numberOfSlots

     ### In this point will be inserted the maintenances
     solution = insertMaintenances(solution, NUMBER_OF_MACHINES, NUMBER_OF_JOBS, p,d,t)
     evaluateCompletionTimes!(solution, NUMBER_OF_MACHINES, p, d, t)
     evaluateSolution!(solution,collect(1:NUMBER_OF_MACHINES), p,d,t)

     println("First solution value: ", solution.solutionValue)

     return solution
end


############### Function to build a initial solution based on expected delay   ##################
######## This function takes the jobs with lower deteriorations and put them first   ############
######## in the machine. It can permit more tasks be done without a maintenance      ############
#################################################################################################
function lowerExpectDelayFirst(NUMBER_OF_JOBS, NUMBER_OF_MACHINES, d, p, t)
     jobOrderedByDeterioration = Array{Pair{Float64, Int32},2}(undef,NUMBER_OF_MACHINES,NUMBER_OF_JOBS)
     solution = Solution()
     solution.slotsOnMachine= zeros(NUMBER_OF_MACHINES)
     solution.maintenancesOnMachine = zeros(NUMBER_OF_MACHINES)
     solution.manteinancePositions = zeros(NUMBER_OF_MACHINES, NUMBER_OF_JOBS)
     solution.machineScheduling = zeros(NUMBER_OF_MACHINES, 2*NUMBER_OF_JOBS)
     solution.machineCompletionTimes = zeros(NUMBER_OF_MACHINES)


     for i in 1:NUMBER_OF_MACHINES
         auxJobsOrdered = Array{Pair{Float64, Int32}}(undef,NUMBER_OF_JOBS)
         for j in 1:NUMBER_OF_JOBS
              auxJobsOrdered[j] = Pair{Float64, Int32}(d[j,i],j)
         end

         sort!(auxJobsOrdered, by = x->x[1])
         jobOrderedByDeterioration[i,:] = auxJobsOrdered
     end

     usedJobs = zeros(NUMBER_OF_JOBS)
     countInserted = 0
     i = 0
    while true
         i += 1
       #  println("___________________________________")
         if i > NUMBER_OF_MACHINES
              i = 1
         end
         for j in 1:NUMBER_OF_JOBS
               if jobUsed[jobOrderedByDeterioration[i,j].second] == 0
                    println(jobOrderedByDuration[i,j].second)
                    usedJobs[jobOrderedByDuration[i,j].second] =1
                    numberOfSlots[i] +=1
                    solution.machineScheduling[i,numberOfSlots[i]] =jobOrderedByDeterioration[i,j].second
                    countInserted +=1
                    break
               end
          end
          if countInserted == NUMBER_OF_JOBS
               break
          end
     end

     ### In this point will be inserted the maintenances
     solution = insertMaintenances(solution, NUMBER_OF_MACHINES, NUMBER_OF_JOBS, p,d,t)
     evaluateCompletionTimes!(solution, NUMBER_OF_MACHINES, p, d, t)

     return solution
end

####################### Function to run a constructive heuristic   ##############################
######## This function takes the jobs with lower deteriorations and put them first   ############
######## in the machine. It can permit more tasks be done without a maintenance      ############
#################################################################################################
function constructiveHeuristic(NUMBER_OF_JOBS, NUMBER_OF_MACHINES, d, p,t)

     if CONSTRUCTIVE_HEURISTIC_STRATEGY == 1
         solution =  Solution(biggerTasksFirst(NUMBER_OF_JOBS, NUMBER_OF_MACHINES, d, p, t))
     elseif CONSTRUCTIVE_HEURISTIC_STRATEGY == 2
         solution =  Solution(lowerExpectDelayFirst(NUMBER_OF_JOBS, NUMBER_OF_MACHINES, d, p, t))
     end

     return solution
end

################################## PEERTUBATIONS ###############################################
function shiftMachineSchedulings(solution, NUMBER_OF_JOBS, NUMBER_OF_MACHINES, d, p, t)
     println("Started")
     if NUMBER_OF_MACHINES> 2
          lastScheduling = solution.machineScheduling[NUMBER_OF_MACHINES,:]

          for i in NUMBER_OF_MACHINES:-1: 2

             #  println(solution.machineScheduling[i,:],solution.machineScheduling[i-1,:])
               solution.machineScheduling[i,:] = solution.machineScheduling[i-1,:]
             #  println(solution.machineScheduling[i,:],solution.machineScheduling[i-1,:])
          end
          solution.machineScheduling[1,:] = lastScheduling
     end

     evaluateSolution!(solution,collect(1:NUMBER_OF_MACHINES), p,d,t)
     return solution
end

##################### Function reverse task job order on machines   ############################
######## This function reverse the job orders in all the machines, recalculating all ###########
######## the maintenances and their respective intervals times                       ###########
################################################################################################
function reverseTasks(initialSolution, NUMBER_OF_JOBS, NUMBER_OF_MACHINES, d, p, t)

     for machine in 1:NUMBER_OF_MACHINES
          completionTime = 0
          deterioration = 1
          job = 1
          numberOfSlots = initialSolution.slotsOnMachine[machine]
          initialSolution.machineScheduling[machine,1:numberOfSlots] = reverse(initialSolution.machineScheduling[machine,1:numberOfSlots])
          index =1
          while index <= numberOfSlots
               job= initialSolution.machineScheduling[machine,index]
               if job > 0
                    completionTime += p[job, machine]
                    deterioration *= d[job,machine]
                    if job < numberOfSlots && (deterioration-1.0)*d[job,machine] >= t[machine]
                         completionTime += t[machine]
                         deterioration = 1
                         numberOfSlots=+1
                         initialSolution.machineScheduling = insertOperation!(initialSolution.machineScheduling[machine,:],
                                                            numberOfSlots,machine, index,-1 )
                    end
                    index+=1
               else
                    initialSolution.machineScheduling[machine,:] = removeOperationAt!(initialSolution.machineScheduling[machine,:],index)
                    numberOfSlots-=1
               end
          end
          initialSolution.slotsOnMachine[machine] = numberOfSlots
     end

     return initialSolution
end


function pertubation(solution,  NUMBER_OF_JOBS, NUMBER_OF_MACHINES, p,d, t)
    global PERTURBATION_STRATEGY
    if PERTURBATION_STRATEGY == 1
          PERTURBATION_STRATEGY = 2
          return shiftMachineSchedulings(solution, NUMBER_OF_JOBS, NUMBER_OF_MACHINES, d, p, t)
     else
          PERTURBATION_STRATEGY = 1
          return reverseTasks(solution, NUMBER_OF_JOBS, NUMBER_OF_MACHINES, d, p, t)
     end

end

function printSolution(initialSolution, NUMBER_OF_MACHINES)

     println("Solution value: ", initialSolution.solutionValue)
     for i in 1:NUMBER_OF_MACHINES
          println("Scheduling in the machine: ",i)

          for j in 1:size(initialSolution.machineScheduling[i,:])[1]
               print(initialSolution.machineScheduling[i,j]," " )
          end
          println("")
     end
     println("Completion Times")
     println(initialSolution.machineCompletionTimes)

     println("______________________________________________")
end

function mainHeuristic(NUMBER_OF_JOBS, NUMBER_OF_MACHINES, p,d, t)

           iterationsWithoutImprovement = 0
           global globalBestSolution = Solution()
           global globalBestSolutionValue = 1e100

           initialSolution = constructiveHeuristic(NUMBER_OF_JOBS, NUMBER_OF_MACHINES,p,d,t)
           println("Initial solution")


           currentSolution = Solution(initialSolution)
           printSolution(initialSolution, NUMBER_OF_MACHINES)
           println("Local searches")
           while iterationsWithoutImprovement < MAX_ITERATIONS_WITHOUT_IMPROVEMENT
                 ### I THINK THIS IF WILL BE NOT NECESSARY
                 #   if totalTime > MAX_EXECUTION_TIME
                 #        break
                 #   end
                 iterationsWithoutImprovement += 1


                 currentSolution = Solution(internal2Swap(currentSolution, NUMBER_OF_JOBS,
                                                          NUMBER_OF_MACHINES, d, p, t))
                 iterationsWithoutImprovement = updateBestGlobalSolution!(currentSolution,
                                                          iterationsWithoutImprovement)
                 println("Current solution A: ", currentSolution.solutionValue)
                 printSolution(currentSolution,NUMBER_OF_MACHINES)

                 currentSolution = Solution(internal3Swap(currentSolution, NUMBER_OF_JOBS,
                                                          NUMBER_OF_MACHINES, d, p, t))
                 iterationsWithoutImprovement = updateBestGlobalSolution!(currentSolution,
                                                          iterationsWithoutImprovement)
                 #printSolution(currentSolution,NUMBER_OF_MACHINES)
                 #println("Current solution B: ", currentSolution.solutionValue)


                 currentSolution = Solution(external2Swap(currentSolution, NUMBER_OF_JOBS,
                                                          NUMBER_OF_MACHINES, d, p, t))
                 iterationsWithoutImprovement = updateBestGlobalSolution!(currentSolution,
                                                          iterationsWithoutImprovement)
                # println("Current solution C: ", currentSolution.solutionValue)
                # printSolution(currentSolution,NUMBER_OF_MACHINES)




                 currentSolution = Solution(external3Swap(currentSolution, NUMBER_OF_JOBS,
                                                          NUMBER_OF_MACHINES, d, p, t))
                 iterationsWithoutImprovement = updateBestGlobalSolution!(currentSolution,
                                                          iterationsWithoutImprovement)
                # println("Current solution D: ", currentSolution.solutionValue)
                # printSolution(currentSolution,NUMBER_OF_MACHINES)



              ##   currentSolution = runTaskBalacing(currentSolution, NUMBER_OF_JOBS, NUMBER_OF_MACHINES, d, p, t)
              ##   iterationsWithoutImprovement = updateBestGlobalSolution(currentSolution, iterationsWithoutImprovement)
                  println("Best value:", globalBestSolutionValue, "\tIterations without improvement: ", iterationsWithoutImprovement)
                  sleep(0.4)

                  if iterationsWithoutImprovement%5 == 1
                      currentSolution =Solution(pertubation(initialSolution,
                                                           NUMBER_OF_JOBS,
                                                            NUMBER_OF_MACHINES, p,d, t))
                     println("___________________PERTUBATION______________")
                     #printSolution(currentSolution,NUMBER_OF_MACHINES)
                     #sleep(1)
                  end


           end
           println("_______________________ FINAL SOLUTION ____________________")
           printSolution(globalBestSolution,NUMBER_OF_MACHINES)
           sleep(10)
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

           for i in 1:NUMBER_OF_MACHINES
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
