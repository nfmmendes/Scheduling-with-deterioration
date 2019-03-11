using JuMP
using CPLEX

################## CREATE A MODEL OBJECT TO BE RUN ON CPLEX  ####################
m = Model(solver=CplexSolver(CPX_PARAM_MIPDISPLAY=1, CPX_PARAM_MIPINTERVAL=1))

MAX_EXECUTION_TIME = 60*15


bigM = 100000
############################ START MAIN FUNCTION ##############################
open("Instances/Instances With Deterioration/instance_list.txt") do file
    #### READING THE INSTANCE LIST
    for instanceName in eachline(file)
        #### MAIN PARAMETERS OF THE OPTIMIZATION
        NUMBER_OF_JOBS = 0
        NUMBER_OF_MACHINES = 0
        NUMBER_OF_PERIODS = 0
        ########################### START TO READ A INSTANCE ###################
        open(string("Instances/Instances With Deterioration/Instances/",instanceName)) do instance
           NUMBER_OF_MACHINES = parse(Int64, readline(instance))
           NUMBER_OF_JOBS = parse(Int64, readline(instance))
           NUMBER_OF_PERIODS = 2*(NUMBER_OF_JOBS-NUMBER_OF_MACHINES+1)+1

           if NUMBER_OF_PERIODS <= 0
              NUMBER_OF_PERIODS = 1
           end

           d = zeros(NUMBER_OF_JOBS, NUMBER_OF_MACHINES)     #2-dimensional array
           p = zeros(NUMBER_OF_JOBS, NUMBER_OF_MACHINES)     #2-dimensional array
           t = zeros(NUMBER_OF_MACHINES)                     #1-dimensional array

           jobDurationData = readline(instance)
           jobDurations = split(jobDurationData, " ")

           ##### STORE JOBS DURATION. IT IS SUPPOSED THAT ALL MACHINES ARE
           ##### EQUALS HERE
           for i in 1:NUMBER_OF_JOBS
               for j in 1:NUMBER_OF_MACHINES
                   p[i,j] = parse(Float64, jobDurations[i])
               end
           end

           maintenanceDurationData = readline(instance)
           maintenanceDuration = split(maintenanceDurationData, " ")

           ##### STORE MAINTENANCE DATA
           for i in NUMBER_OF_MACHINES
               t[i] = parse(Float64, maintenanceDuration[i])
           end

           #### STORE DETERIORATION DATA. IT IS SUPPOSED THAT THE MACHINES ARE
           #### DIFERENT HERE
           for i in 1:NUMBER_OF_JOBS
                 stringData = readline(instance)
                 rowData = split(stringData, " ")
                 for j in 1:NUMBER_OF_MACHINES
                     d[i,j] = parse(Float64, rowData[j])
                end
           end

           ##################################### MODEL CREATION #######################################

           #Binary variable. Defines if the job j is assigned to machine k at position h
           @variable(m,x[1:NUMBER_OF_JOBS, 1:NUMBER_OF_MACHINES, 1: NUMBER_OF_PERIODS]>=0, Bin,basename="x")

           #Binary variable. Defines if there is a maintenance service on machine k at position h
           @variable(m,s[1:NUMBER_OF_MACHINES, 1: NUMBER_OF_PERIODS]>=0, Bin,basename="s")

           #Continuous variable. Defines the delay in finishing the job assigned at h-esime position of machine k
           @variable(m,q[1:NUMBER_OF_MACHINES, 1: NUMBER_OF_PERIODS]>=0,basename="q")

           #Continuous variable. Define the time spent to execute the job j on the machine k at position h
           @variable(m, a[1:NUMBER_OF_JOBS, 1:NUMBER_OF_MACHINES, 1:NUMBER_OF_PERIODS], basename="a")

           #Continuous variable. Defines the makespan
           @variable(m, Cmax, basename="Cmax")


           ############################################################################################
           ##      CONSTRAINT 1: A JOB AND A MAINTENANCE CANNOT BE DONE IN THE SAME MACHINE AT
           ##                                    SAME POSITION
           ############################################################################################
           for h = 1:NUMBER_OF_PERIODS
               for k = 1:NUMBER_OF_MACHINES
                  @constraint(m, sum(x[j,k,h] for j in 1:NUMBER_OF_JOBS) +s[k,h] <= 1 )
               end
           end

           ############################################################################################
           ##           CONSTRAINT 2: A JOB MUST BE DONE IN ONE MACHINE AND ONE POSITION
           ############################################################################################
           for j= 1:NUMBER_OF_JOBS
               @constraint(m, sum(x[j,k,h] for k in 1:NUMBER_OF_MACHINES, h in 1:NUMBER_OF_PERIODS) == 1)
           end

           ############################################################################################
           ##           CONSTRAINT 3: TWO MAINTENANCES CANNOT BE DONE IN CONSECUTIVE POSITIONS
           ############################################################################################
           for k =1:NUMBER_OF_MACHINES
               for h = 2:NUMBER_OF_PERIODS
                  @constraint(m, s[k,h-1] + s[k,h] <= 1)
               end
           end


           ############################################################################################
           ##           CONSTRAINT 4: IF WE DON'T HAVE ACTIVITIES IN THE PREVIOUS POSITION IN ONE
           ##                  MACHINE IT WILL BE NOT ANY ACTIVITY IN THE CURRENT PERIOD.
           ##                      THIS AVOIDS EMPTY POSITIONS INSIDE THE SCHEDULE
           ############################################################################################
           for k =1:NUMBER_OF_MACHINES
               for h = 2:NUMBER_OF_PERIODS
                  @constraint(m, s[k,h-1] + sum(x[j,k,h-1] for j in 1:NUMBER_OF_JOBS)>= s[k,h] + sum(x[j,k,h] for j in 1:NUMBER_OF_JOBS) )
               end
           end

           ############################################################################################
           ##           CONSTRAINT 5: JUST ONE JOB CAN BE DONE IN THE POSITION H ON MACHINE K
           ############################################################################################
           for k =1:NUMBER_OF_MACHINES
               for h = 1:NUMBER_OF_PERIODS
                  @constraint(m, sum(x[j,k,h] for j in 1:NUMBER_OF_JOBS) <= 1 )
               end
           end


           ############################################################################################
           ##           CONSTRAINT 6: THE JOB J HAS A ZERO EXECUTION TIME ON MACHINE K AND
           ##              POSITION H IF IT IS NOT DONE IN MACHINE K AND POSITION H
           ############################################################################################
           for j = 1:NUMBER_OF_JOBS
               for k = 1:NUMBER_OF_MACHINES
                  for h = 1:NUMBER_OF_PERIODS
                       @constraint( m,  a[j,k,h] <= bigM*x[j,k,h])
                  end
               end
           end



           ############################################################################################
           ##           CONSTRAINT 7: DEFINE THE TIME NEEDED TO EXECUTE THE JOB J ON MACHINE
           ##              K AT THE PERIOD H BASED ON DELAYS CAUSED BY DETERIORATION
           ############################################################################################
           for j = 1:NUMBER_OF_JOBS
               for k = 1:NUMBER_OF_MACHINES
                  for h = 1:NUMBER_OF_PERIODS
                       @constraint( m,  a[j,k,h] >= p[j,k]*q[k,h] - bigM*(1-x[j,k,h]))
                  end
               end
           end

           ############################################################################################
           ##                         CONSTRAINT 8: DEFINE THE SCHEDULING MAKESPAN
           ############################################################################################
           for k = 1:NUMBER_OF_MACHINES
               @constraint(m,  sum(a[j,k,h] for j in 1:NUMBER_OF_JOBS, h in 1:NUMBER_OF_PERIODS) +
                             sum(t[k]*s[k,h] for h in 1:NUMBER_OF_PERIODS) - Cmax<= 0)
           end


           ############################################################################################
           ##                 CONSTRAINT 9: INTEGRITY CONSTRAINT SIMILAR TO CONSTRAINT 4
           ############################################################################################
           for j= 1:NUMBER_OF_JOBS
               for k = 1:NUMBER_OF_MACHINES
                  for h = 2:NUMBER_OF_PERIODS
                       @constraint(m, x[j,k,h] - sum(x[jj,k,h-1] for jj in 1:NUMBER_OF_MACHINES) -s[k,h-1] <= 0)
                  end
               end
           end


           ############################################################################################
           ##                 CONSTRAINT 10: THERE IS NOT DELAY AFTER AN MAINTENANCE
           ############################################################################################
           for k = 1:NUMBER_OF_MACHINES
               for h = 2:NUMBER_OF_PERIODS
                  @constraint(m, q[k,h] <= bigM*(1-s[k,h-1])+1)
               end
           end

           ############################################################################################
           ##                 CONSTRAINT 11: THERE IS A DELAY AFTER EACH JOB
           ############################################################################################
           for j= 1:NUMBER_OF_JOBS
               for k = 1:NUMBER_OF_MACHINES
                  for h = 2:NUMBER_OF_PERIODS
                       @constraint(m, q[k,h] >= d[j,k]*q[k,h-1] - bigM*(s[k,h-1] + (1- x[j,k,h])))
                  end
               end
           end

           ############################################################################################
           ##                 CONSTRAINT 12: Q VALUE INITIALIZATION
           ############################################################################################
           for k=1:NUMBER_OF_MACHINES
               for h = 1:NUMBER_OF_PERIODS
                       @constraint(m, q[k,h] >= 1)
               end
           end

           ############################################################################################
           ##                 CONSTRAINT 12: Q VALUE INITIALIZATION 2
           ############################################################################################
           for k = 1:NUMBER_OF_MACHINES
               @constraint(m, q[k,1] == 1)
           end

           #@constraint(m, s[1,2] == 1)
           @constraint(m, Cmax >= 0)
           for j = 1:NUMBER_OF_JOBS
               for k = 1:NUMBER_OF_MACHINES
                  for h = 1:NUMBER_OF_PERIODS
                       @constraint(m, a[j,k,h] >= 0 )
                  end
               end
           end


           ############################################################################################
           ##                              OBJECTIVE FUNCTION
           ############################################################################################
           @objective(m, Min, Cmax)

           println("Starting optimization");

           #print(m)

          # write("output.txt", readstring(`ls -l`))

           tic()
           st = solve(m)
           toc()

           ############################################################################################
           ##                              PRINTING SOLUTION
           ############################################################################################

           if(st == :Optimal)
               ############################################################################################
               ##                              PRINTING CMAX VARIABLE VALUE
               ############################################################################################
               println(string("Cmax ", getValue(Cmax)))

               ############################################################################################
               ##                              PRINTING X VARIABLE VALUES
               ############################################################################################
               for j = 1:NUMBER_OF_JOBS
                  for k = 1:NUMBER_OF_MACHINES
                       for h = 1:NUMBER_OF_PERIODS
                           if ( getvalue(x[j,k,h] ) >  0)
                               println(string("x(", j, ",", k , ",", h,") " , getvalue(x[j,k,h])))
                           end
                       end
                    end
               end

               ############################################################################################
               ##                              PRINTING A VARIABLE VALUES
               ############################################################################################
               for j = 1:NUMBER_OF_JOBS
                  for k = 1:NUMBER_OF_MACHINES
                       for h = 1:NUMBER_OF_PERIODS
                           if ( getvalue(a[j,k,h] ) > 0)
                               println(string("a(", j, ",", k , ",", h,") " , getvalue(a[j,k,h])))
                           end
                       end
                    end
               end

               ############################################################################################
               ##                              PRINTING Q VARIABLE VALUES
               ############################################################################################
               for k = 1:NUMBER_OF_MACHINES
                  for h = 1:NUMBER_OF_PERIODS
                       if ( getvalue(q[k,h] ) > 0)
                           println(string("q(", k , ",", h,") " , getvalue(q[k,h])))
                       end
                  end
                end


                ############################################################################################
                ##                              PRINTING S VARIABLE VALUES
                ############################################################################################
                for k = 1:NUMBER_OF_MACHINES
                    for h = 1:NUMBER_OF_PERIODS
                       if ( getvalue(s[k,h] ) >  0)
                           println(string("s(", k , ",", h,") " , getvalue(s[k,h])))
                       end
                    end
                end
           end
        end
    end
end
