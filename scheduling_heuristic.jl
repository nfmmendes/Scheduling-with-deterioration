using JuMP
using CPLEX

m = Model(solver=CplexSolver(CPX_PARAM_MIPDISPLAY=1, CPX_PARAM_MIPINTERVAL=1))

#####################      HEURISTIC PARAMETERS     #######################

###############################################################################
### THESE ARE THE DEFAULT VALUES. THEY CAN BE CHANGED IN ANY PART OF THE CODE
###############################################################################
MAX_ITERATIONS_WITHOUT_IMPROVEMENT = 50
MAX_EXECUTION_TIME = 60*30
SWAP_UPDATE_STRATEGY = 1  # 1 = ALWAYS THE SAME CURRENT SOLUTION
                          # 2 = UPDATE CURRENT SOLUTION IN EACH MACHINE
                          # 3 = UPDATE CURRENT SOLUTION AFTER GLOBAL IMPROVEMENT
MAX_LOCAL_SEARCH_SIZE = 5000
#############################################################################

struct Solution
      maintenancePositions          #2-dimensional array to define the slots in
                                    #each machine where maintenances are done.
                                    #First index indicates the machine

     interMaintenanceTimes         #Stores the time between two maintentances.
                                    #If none task is changed in the interval this
                                    #times remains constant

      machineCompletionTimes       #Stores the completion time in each machine

      machineScheduling             #2-dimensional array to define the activity
                                    #done in each slot in each machine
                                    #First index indicates the machine
                                    #An positive index (> 0) represent the job index.
                                    #Otherwise we have a maintenance, represented by the value -1

     solutionValue::Float64         #Stores solution objective function value

end

globalBestSolution = solution()
globalBestSolutionValue = 1e1000

function evaluateSolution(currentSolution,firstSwapSlot,firstSwapSlot,-1)

end


function updateBestSolutionsOnSwapLS(initialSolution, previousSolution, solutionValue, currentSolution, bestSolution, bestSolutionOnMachineLoop)
     #### Evaluate if it is the best solution in the local search
     if solutionValue < bestSolutionValue
        bestSolution.solutionValue = solutionValue
        bestSolution = Solution(currentSolution)
     end

     #### Evaluate if it is the best solution in the local search and current machine
     if SWAP_UPDATE_STRATEGY == 2 && solutionValue < bestSolutionOnMachineLoop.solutionValue
          bestSolutionOnMachineLoop = Solution(currentSolution)
     else if SWAP_UPDATE_STRATEGY == 2
          currentSolution = Solution(previousSolution)
     end

     ### Current solution update strategy
     if SWAP_UPDATE_STRATEGY == 1        ## Never update the current solution
          currentSolution = previousSolution
          continue
     else if SWAP_UPDATE_STRATEGY == 3   ## Update the current solution if it is the best
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

################################ Function to test swaps between JOBS #########################
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
                   firstSwapSlot +=1     # We don't need to verify if it is a maintenance
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
                end

                lastSwapSlot = firstSwapSlot + 1
           end

           ## If the second slot is a maintenance, we should go to the next slot
           ## To shortness of the code, we use the 'continue' statement to make all
           ## the swap validity verification
           if solution.machineScheduling[machine,lastSwapSlot] == -1
                lastSwapSlot += 1
                continue
           end

           ###  Effectivelly do the swap
           previousSolution = Solution(currentSolution)
           firstJob = currentSolution.machineScheduling[machine,lastSwapSlot]
           currentSolution.machineScheduling[machine,lastSwapSlot] = currentSolution.machineScheduling[machine,firstSwapSlot]
           currentSolution.machineScheduling[machine,firstSwapSlot] = firstJob

           solutionValue = evaluateSolution(currentSolution,firstSwapSlot,lastSwapSlot,-1)
           updateBestSolutionsOnSwapLS(initialSolution, previousSolution, solutionValue, currentSolution, bestSolution, bestSolutionOnMachineLoop)
           lastSwapSlot = lastSwapSlot+1
     end
end

function internal3Swap(initialSolution, NUMBER_OF_JOBS, NUMBER_OF_MACHINES, d, p, t)
           bestSolutionValue = 1e100
           currentSolution = initialSolution
           triedAll = false
           localSeachSize =0
           currentMachine = 1
           firstSwapSlot = 1
           secondSwapSlot =2
           thirdSwapSlot = 3



           while triedAll == false || localSeachSize < MAX_LOCAL_SEARCH_SIZE

                 localSeachSize +=1

                 ## All the possible swaps with the "currentFirstSlot" were already done
                 if thirdSwapSlot > numberOfPeriodsOnMachine
                      secondSwapSlot+=1

                      if secondSwapSlot > numberOfPeriodsOnMachine-1
                           firstSwapSlot +=1

                           ## There is no need to put a "while" loop here because it's not
                           ## allowed two consective maintenances without a job in between
                           if currentSolution.machineScheduling[machine,firstSwapSlot] == -1
                              firstSwapSlot+=1
                           end

                           ### All the swaps in the current machine were already tested
                           ### so we should go to the next one
                           if firstSwapSlot >= numberOfPeriodsOnMachine-1
                              machine +=1
                              firstSwapSlot +=1     # We don't need to verify if it is a maintenance
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
                           end

                           secondSwapSlot = firstSwapSlot + 1
                      end

                      if currentSolution.machineScheduling[machine,secondSwapSlot] == -1
                        secondSwapSlot+=1
                      end
                 end

                 ## If the second slot is a maintenance, we should go to the next slot
                 ## To shortness of the code, we use the 'continue' statement to make all
                 ## the swap validity verification
                 if solution.machineScheduling[machine,thirdSwapSlot] == -1
                      lastSwapSlot += 1
                      continue
                 end

                 ###  Effectivelly do the swap
                 previousSolution = Solution(currentSolution)
                 firstJob = currentSolution.machineScheduling[machine,lastSwapSlot]
                 currentSolution.machineScheduling[machine,lastSwapSlot] = currentSolution.machineScheduling[machine,firstSwapSlot]
                 currentSolution.machineScheduling[machine,firstSwapSlot] = firstJob

                 solutionValue = evaluateSolution(currentSolution,firstSwapSlot,secondSwapSlot,thirdSwapSlot)
                 updateBestSolutionsOnSwapLS(initialSolution, previousSolution, solutionValue, currentSolution, bestSolution, bestSolutionOnMachineLoop)
           end
























           solutionValue = evaluateSolution(currentSolution,firstSwapSlot,lastSwapSlot,-1)
           updateBestSolutionsOnSwapLS(initialSolution, previousSolution, solutionValue, currentSolution, bestSolution, bestSolutionOnMachineLoop)

end

function external2Swap(initialSolution, NUMBER_OF_JOBS, NUMBER_OF_MACHINES, d, p, t)

end

function external3Swap(initialSolution, NUMBER_OF_JOBS, NUMBER_OF_MACHINES, d, p, t)

end

function runTaskBalacing(initialSolution, NUMBER_OF_JOBS, NUMBER_OF_MACHINES, d, p, t)

end

#################################### INITIAL SOLUTIONS STRATEGIES ##############################
function biggerTasksFirst(initialSolution, NUMBER_OF_JOBS, NUMBER_OF_MACHINES, d, p, t)

end

function biggerExpectDelayFirst(initialSolution, NUMBER_OF_JOBS, NUMBER_OF_MACHINES, d, p, t)

end

################################## PEERTUBATIONS ###############################################
function swapExtremeTasks(initialSolution, NUMBER_OF_JOBS, NUMBER_OF_MACHINES, d, p, t)

end

function InvertTasks(initialSolution, NUMBER_OF_JOBS, NUMBER_OF_MACHINES, d, p, t)

end


function pertubation(initialSolution,  NUMBER_OF_JOBS, NUMBER_OF_MACHINES, d, p, t)

end

function mainHeuristic(NUMBER_OF_JOBS, NUMBER_OF_MACHINES, d, p, t)

           iterationsWithoutImprovement = 0

           while iterationsWithoutImprovement < MAX_ITERATIONS_WITHOUT_IMPROVEMENT
                 ### I THINK THIS IF WILL BE NOT NECESSARY
                 if totalTime > MAX_EXECUTION_TIME
                      break
                 end

                 internal2Swap(initialSolution, NUMBER_OF_JOBS, NUMBER_OF_MACHINES, d, p, t)
                 internal3Swap(initialSolution, NUMBER_OF_JOBS, NUMBER_OF_MACHINES, d, p, t)
                 external2Swap(initialSolution, NUMBER_OF_JOBS, NUMBER_OF_MACHINES, d, p, t)
                 external3Swap(initialSolution, NUMBER_OF_JOBS, NUMBER_OF_MACHINES, d, p, t)
                 runTaskBalacing(initialSolution, NUMBER_OF_JOBS, NUMBER_OF_MACHINES, d, p, t)

                  if iterationsWithoutImprovement%10 == 9
                      pertubation(initialSolution,  NUMBER_OF_JOBS, NUMBER_OF_MACHINES, d, p, t)
                  end
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
        end
    end
end
