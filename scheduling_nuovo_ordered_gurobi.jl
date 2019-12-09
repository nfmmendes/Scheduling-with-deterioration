using JuMP
using Gurobi
using MathOptFormat


MAX_EXECUTION_TIME = 60*15


bigM = 100000
############################ START MAIN FUNCTION ##############################
open("Instances/Instances With Deterioration/test_gurobi.txt") do file
    #### READING THE INSTANCE LIST
    for instanceName in eachline(file)
        ################## CREATE A MODEL OBJECT TO BE RUN ON CPLEX  ####################
        model = Model(with_optimizer(Gurobi.Optimizer))
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
		   lowestJob = 1e8
           for i in 1:NUMBER_OF_JOBS
               for j in 1:NUMBER_OF_MACHINES
                   p[i,j] = parse(Float64, jobDurations[i])
				   if p[i,j] < lowestJob 
					  lowestJob = p[i,j]
					end
               end
           end

           maintenanceDurationData = readline(instance)
           maintenanceDuration = split(maintenanceDurationData, " ")

           ##### STORE MAINTENANCE DATA
		   biggerMaintenance = -1
           for i in 1:NUMBER_OF_MACHINES
               t[i] = parse(Float64, maintenanceDuration[i])
			   if t[i] > biggerMaintenance
					biggerMaintenance = t[i]
			   end
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

           #Binary variable. Defines if the job j is assigned after the job i to machine m
           @variable(model,x[1:(3+NUMBER_OF_JOBS), 1:(3+NUMBER_OF_JOBS), 1:NUMBER_OF_MACHINES], Bin,base_name="x")

           #Continuous variable. Defines the delay factor after the job j
           @variable(model,q[1:NUMBER_OF_JOBS+2]>=0,base_name="q")

           #Continuous variable. Define the time spent to execute the job j on the machine k
           @variable(model, a[1:NUMBER_OF_JOBS+1, 1:NUMBER_OF_MACHINES] >= 0, base_name="a")

           #Continuous variable. Define the completion time on machine k on period h
        #   @variable(model, f[1:NUMBER_OF_JOBS+2], base_name="f")

           #Continuous variable. Defines the makespan
           @variable(model, Cmax, base_name="Cmax")


			###### Q[a1] is a acccumulated deterioration and a2 and a3 tasks, a optimal sequence must obey
			###### Q[a1]*p[a2] + Q[a1]*d[a2]*p[a3] <= Q[a1]*p[a3]+ Q[a1]*d[a3]*p[a2]
			###### p[a2] + d[a2]*p[a3] <= p[a3] + d[a3]*p[a2]
			###### 
			
			for i in 2:(NUMBER_OF_JOBS+1)
				for j in (2:NUMBER_OF_JOBS+1)
					for k in (1:NUMBER_OF_MACHINES)
						if p[i-1,k] + d[i-1,k]*p[j-1,k] > p[j-1,k] + d[j-1,k]*p[i-1,k]
						  @constraint(model, x[i,j,k] == 0)
						end
					end
				end
			end



           ############################################################################################
           ##      CONSTRAINT 1: A JOB SHOULD BE DONE IN SOME MACHINE AND AFTER SOME JOB, MAINTENANCE
           ##                                    OR STARTING POINT
           ############################################################################################
            for j = 2:(NUMBER_OF_JOBS+1)
                @constraint(model, sum( x[i,j,k] for i = 1:(NUMBER_OF_JOBS+2), k = 1:NUMBER_OF_MACHINES) == 1 )
            end



           ############################################################################################
           ##      CONSTRAINT 2: FOR EACH MACHINE, JUST ONE JOB CAN BE AFTER THE STARTING POINT AND 
		   ##                    BEFORE A END POINT
           ############################################################################################
            for k = 1:(NUMBER_OF_MACHINES)
                @constraint(model, sum( x[1,j,k] for j = 2:(NUMBER_OF_JOBS+1)) <= 1 )
            end

			for k = 1:(NUMBER_OF_MACHINES)
                @constraint(model, sum( x[j,NUMBER_OF_JOBS+3,k] for j = 2:(NUMBER_OF_JOBS+1)) <= 1 )
            end

           ############################################################################################
           ##           CONSTRAINT 3: CONSTRAINT ANTI-CYCLE. A FLOW STARTS FROM ONE JOB AND DECREASES
           ##               ONE UNITY IN EACH JOB. IF THE JOB CLOSES A CYCLE THIS CONSTRAINT IS NOT
           ##               RESPECTED. A CYCLE CAN START AND FINISH IN A MAINTENANCE HOWEVER
           ############################################################################################
           for j= 2:NUMBER_OF_JOBS+1
               for i=1:(NUMBER_OF_JOBS+2)
					for k=1:NUMBER_OF_MACHINES
                #   		@constraint(model, f[j] <= f[i]-1 + 2.1*(NUMBER_OF_JOBS)*(1- sum(x[i,j,k] ) ) )
					end
               end
           end


		   for j= 2:NUMBER_OF_JOBS+1
               for i=1:(NUMBER_OF_JOBS+2)
					for k=1:NUMBER_OF_MACHINES
	             #      @constraint(model, f[j] >= f[i]-1 - 2.1*(NUMBER_OF_JOBS)*(1- sum(x[i,j,k] ) ) )
					end
               end
           end


            #@constraint(model, f[1]== 2*(NUMBER_OF_JOBS+3))
            #@constraint(model, f[NUMBER_OF_JOBS+2]== 2*(NUMBER_OF_JOBS+3))

           ############################################################################################
           ##           CONSTRAINT 4: THERE MUST THE SAME NUMBER OF JOBS BEFORE AND ONE AFTER THE JOB J
           ##               ON A MACHINE
           ############################################################################################
           for j =2:(NUMBER_OF_JOBS+2)
				for m =1:NUMBER_OF_MACHINES
                	@constraint(model, sum(x[i,j,m] for i = 1:(NUMBER_OF_JOBS+2)) == sum(x[j,i,m] for i = 2:(NUMBER_OF_JOBS+3) ) )
				end
           end

		

           ############################################################################################
           ##           CONSTRAINT 5: DELAY FACTOR EVALUATION
           ############################################################################################
           for i = 1:(NUMBER_OF_JOBS+2)
                for j=2:(NUMBER_OF_JOBS+1)
                    for m = 1:NUMBER_OF_MACHINES
                        if i == 1 || i == NUMBER_OF_JOBS +2
                            @constraint(model, q[j]  >= q[i] - (1+biggerMaintenance/lowestJob) *(1-x[i,j,m]) )
                        else ## THE FIRST INDEX OF d GOES FROM 1 TO NUMBER OF JOBS. SO WE NEED TO DO -1
							##println(string("i = ", i, " j= ", j, " m = ", m, " d[i-1,m] = ", d[i-1,m]))
                            @constraint(model, q[j]  >= q[i]*d[i-1,m] - 100*(1+biggerMaintenance/lowestJob)*(1-x[i,j,m]) )
                        end
                    end
                end
            end

            @constraint(model, q[1] == 1 )
            @constraint(model, q[NUMBER_OF_JOBS+2] == 1)

            ############################################################################################
            ##           CONSTRAINT 6: PROCESSING TIME
            ############################################################################################
            for i = 1:(NUMBER_OF_JOBS+2)
                for j=2:(NUMBER_OF_JOBS+1)
                    for m=1:NUMBER_OF_MACHINES
                        @constraint(model,a[j,m] >= p[j-1,m]*q[i] - (1+biggerMaintenance/lowestJob)*p[j-1,m]*(1-x[i,j,m]))
                    end
                end
            end

			for j=2:(NUMBER_OF_JOBS+1)
				#@constraint(model, q[j] <= 1+(biggerMaintenance/lowestJob) + 100*(1-sum(x[j,i,m] for i=2:(NUMBER_OF_JOBS+1), m=1:NUMBER_OF_MACHINES)) )
				for m=1:NUMBER_OF_MACHINES
					@constraint(model, a[j,m] - p[j-1,m] <= t[m])
				end
			end

            ############################################################################################
            ##           CONSTRAINT 7: MAKESPAN
            ############################################################################################
            for m=1:NUMBER_OF_MACHINES
                @constraint(model, sum(a[j,m] for j = 2:(NUMBER_OF_JOBS+1) ) +
                                   sum(t[m]*x[i,NUMBER_OF_JOBS+2,m] for i=1:(NUMBER_OF_JOBS+1) ) <= Cmax)
            end

			############################################################################################
            ##           CONSTRAINT 8: AVOID A UNIQUE CICLE DEPARTING AND RETURNING TO A MAINTENANCE
            ############################################################################################
			for m=1:NUMBER_OF_MACHINES
				@constraint(model, sum(x[i,j,m] for i=2:(NUMBER_OF_JOBS+1), j=2:(NUMBER_OF_JOBS+2))<= 
								   NUMBER_OF_JOBS*NUMBER_OF_JOBS*sum(x[1,j,m] for j=2:(NUMBER_OF_JOBS+2)))
			end	



            @constraint(model, [i = 1:(NUMBER_OF_JOBS+3), k=1:NUMBER_OF_MACHINES],  x[i,i,k] == 0)
            @constraint(model, [i = 1:(NUMBER_OF_JOBS+3), k=1:NUMBER_OF_MACHINES],  x[i,1,k] == 0)
            @constraint(model, [i = 1:(NUMBER_OF_JOBS+3), k=1:NUMBER_OF_MACHINES],  x[NUMBER_OF_JOBS+3,i,k] == 0)
            @constraint(model, [i = 1:(NUMBER_OF_JOBS+2)],  q[i] >= 1)


           ############################################################################################
           ##                              OBJECTIVE FUNCTION
           ############################################################################################
           @objective(model, Min, Cmax )

           println("Starting optimization: ", instanceName);
           flush(stdout)

        #   print(m)

          # write("output.txt", readstring(`ls -l`))

           @time begin
			#	writeLP(model, string("LP_FILES/", instanceName))
         		#  st = optimize!(model)
				lp_file = MathOptFormat.LP.Model()
				MOI.copy_to(lp_file, backend(model))
				MOI.write_to_file(lp_file, string("LP_Files/", replace(instanceName, "txt"=> "lp") ))
           end

           println(termination_status(model))

           ############################################################################################
           ##                              PRINTING SOLUTION
           ############################################################################################
           println("Optimization finished")
           if(termination_status(model) == MOI.OPTIMAL)
               ############################################################################################
               ##                              PRINTING CMAX VARIABLE VALUE
               ############################################################################################
               println(string("Cmax ", value(Cmax)))

               ############################################################################################
               ##                              PRINTING X VARIABLE VALUES
               ############################################################################################
               for i = 1:(NUMBER_OF_JOBS+3)
                  for j = 1:(NUMBER_OF_JOBS+3)
                       for k = 1:NUMBER_OF_MACHINES
                           if ( value(x[i,j,k] ) >  0.1)
                               println(string("x(", i, ",", j , ",", k,") " , value(x[i,j,k])))
                           end
                       end
                    end
               end

               ############################################################################################
               ##                              PRINTING A VARIABLE VALUES
               ############################################################################################
               for j = 1:NUMBER_OF_JOBS
                    for k = 1:NUMBER_OF_MACHINES
                       if ( value(a[j,k] ) > 0)
                           println(string("a(", j, ",",k,") " , value(a[j,k])))
                       end
                    end
               end

               ############################################################################################
               ##                              PRINTING Q VARIABLE VALUES
               ############################################################################################
               for j = 1:(NUMBER_OF_JOBS+2)
                   if ( value(q[j] ) > 0)
                       println(string("q(", j , ") " , value(q[j])))
                   end
                end


			   ############################################################################################
               ##                              PRINTING F VARIABLE VALUES
               ############################################################################################
               for j = 1:(NUMBER_OF_JOBS+1)
                   if ( value(f[j] ) > 0)
                       println(string("f(", j , ") " , value(f[j])))
                   end
                end

           elseif termination_status(model) == MOI.TIME_LIMIT && has_values(model)
                println("-------TIME LIMIT REACHED. BEST VALUE FOUND: ", objective_value(model))
                println(" ")
                println(" ")
                println(" ")
           else
                println("-------UNEXPECTED ERROR : ", instanceName )
                println(" ")
                println(" ")
                println(" ")
           end
           flush(stdout)
        end
    end
end
