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
bestSolution = []


 mutable struct Group
         jobSequence::Array{Int32}
         deterioration::Float64
         value::Float64
         Group()  =new([],1,0)
end



function evaluateGroups(groups,NUMBER_OF_MACHINES, Ws, p, d, t)

    makespan = 0

    for i in 1:NUMBER_OF_MACHINES
        completionTime = groups[i,1].value

        for j in 2:Ws
            if groups[i,j].value > 1e-05
                completionTime += t[i]
                completionTime += groups[i,j].value
            end
        end
        if completionTime > makespan
            makespan = completionTime
        end

    end

    return makespan
end

function mainHeuristic(NUMBER_OF_JOBS, NUMBER_OF_MACHINES, p,d, t)

           global globalBestSolutionValue
           Ws = NUMBER_OF_MACHINES


           ##### ORDER JOBS BY PROCESSING TIME (DURATION)
           jobsOrderedByDuration = Array{Pair{Float64, Int32},2}(undef,NUMBER_OF_MACHINES,NUMBER_OF_JOBS)
           for i in 1:NUMBER_OF_MACHINES
                 auxJobsOrdered = Array{Pair{Float64, Int32}}(undef,NUMBER_OF_JOBS)

                 for j in 1:NUMBER_OF_JOBS
                       auxJobsOrdered[j] = Pair{Float64, Int32}(p[j,i],j)
                 end

                 sort!(auxJobsOrdered, by = x->x[1], rev=true)
                 jobsOrderedByDuration[i,:] = auxJobsOrdered
           end

            ##### ORDER JOBS BY DETERIORATION FACTOR  (DELAY)
           jobsOrderedByDeterioration = Array{Pair{Float64, Int32},2}(undef,NUMBER_OF_MACHINES,NUMBER_OF_JOBS)
           for i in 1:NUMBER_OF_MACHINES
                 auxJobsOrdered = Array{Pair{Float64, Int32}}(undef,NUMBER_OF_JOBS)

                 for j in 1:NUMBER_OF_JOBS
                       auxJobsOrdered[j] = Pair{Float64, Int32}(d[j,i],j)
                 end

                 sort!(auxJobsOrdered, by = x->x[1])
                 jobsOrderedByDeterioration[i,:] = auxJobsOrdered
           end


           while true

                 G = Array{Group,2}(undef,NUMBER_OF_MACHINES, Ws)
                 for i in 1:NUMBER_OF_MACHINES
                     for j in 1:Ws
                            G[i,j] =  Group()
                     end
                 end


                 CmaxBEST= 1e100
                 numberOfUsedJobs = 0
                 usedJobs = zeros(NUMBER_OF_JOBS)
                 usedMachines = zeros(NUMBER_OF_MACHINES)
                 usedGroups = 0

                 machineIndex = Array{Int32}(undef,NUMBER_OF_MACHINES)
                 for i in 1:NUMBER_OF_MACHINES
                   machineIndex[i] = 1
                 end

                 while numberOfUsedJobs < NUMBER_OF_JOBS

                      quickerMachine = -1
                      quickerJob = -1
                      value = 1e100

                      for i = 1:NUMBER_OF_MACHINES

                            if sum(G[i,m].value for m in 1:Ws) +jobsOrderedByDuration[i,machineIndex[i]][1] < value  &&
                               usedJobs[jobsOrderedByDuration[i,machineIndex[i]][2]] <1

                                       value = sum(G[i,m].value for m in 1:Ws) +jobsOrderedByDuration[i,machineIndex[i]][1]
                                       quickerMachine = i
                                       quickerJob = jobsOrderedByDuration[i,machineIndex[i]][2]
                           end
                      end

                    numberOfUsedJobs +=1
                    usedJobs[quickerJob] = 1
                    machine = quickerMachine
                    jobPos = machineIndex[machine]

                    for i =1:NUMBER_OF_MACHINES
                        if  usedJobs[jobsOrderedByDuration[i,machineIndex[i]][2]] > 0
                            machineIndex[i] = machineIndex[i]+1
                        end
                    end

                    lowestCompletion = 1e100
                    bestGroup = -1

                    for i in 1:Ws

                        runtime = G[machine,i].deterioration * jobsOrderedByDuration[machine,jobPos][1]
                        ## New group, in an used machine, if it's possible use other group
                        if G[machine,i].value < 1e-05 && usedMachines[machine] >0 && usedGroups < Ws &&
                           G[machine,i].value + t[machine] +runtime < lowestCompletion
                            lowestCompletion = G[machine,i].value + t[machine] + runtime
                            bestGroup = i
                        elseif G[machine,i].value < 1e-05 && usedGroups < Ws &&
                           G[machine,i].value +runtime < lowestCompletion
                            lowestCompletion = G[machine,i].value + runtime
                            bestGroup = i
                        elseif G[machine,i].value + runtime < lowestCompletion
                            lowestCompletion = G[machine,i].value + runtime
                            bestGroup = i
                        end
                    end

                    push!(G[machine,bestGroup].jobSequence,quickerJob)
                    G[machine,bestGroup].deterioration *= d[quickerJob,machine]

                    if G[machine,bestGroup].value < 1e-05
                        G[machine,bestGroup].value = lowestCompletion - t[machine]
                        usedGroups = usedGroups+=1
                    else
                        G[machine,bestGroup].value = lowestCompletion
                    end

                    if usedMachines[machine] == 0
                        usedGroups +=1
                    end

                    usedMachines[machine] = 1
                 end

                 makespan = evaluateGroups(G, NUMBER_OF_MACHINES,Ws,p,d,t)
                 if makespan < globalBestSolutionValue
                    globalBestSolutionValue = makespan
                    bestSolution = G
                 end

                 if Ws >= NUMBER_OF_JOBS - NUMBER_OF_MACHINES
                    break
                 end
                 Ws +=1
           end
        println("....." , globalBestSolutionValue)
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
           global globalBestSolutionValue = 1e100
           mainHeuristic(NUMBER_OF_JOBS, NUMBER_OF_MACHINES,p,d,t)
        end
    end
end
