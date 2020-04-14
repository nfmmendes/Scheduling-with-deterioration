#include<iostream>
#include<string>
#include<cmath>
#include<vector>
#include<set> 
#include<map>
#include<fstream> 
#include<iomanip>
#include<numeric>
#include<cassert>
#include<chrono>
#include<algorithm> 
using namespace std;
using namespace std::chrono;

#define MAX_ITERATIONS_WITHOUT_IMPROVEMENT 20 
#define MAX_SWAPS_FACTOR_ATTEMPTS 5 
#define MAX_MOVE_JOBS_ATTEMPTS 10
#define NUM_SWAP_GROUPS_LOCAL_SEARCH_REPETITIONS 10
#define NUM_SWAP_JOBS_LOCAL_SEARCH_REPETITIONS 30
#define NUM_MOVE_JOB_TO_OTHER_INTERNAL_GROUP_REPETITIONS 30
#define NUM_MOVE_JOB_TO_OTHER_EXTERNAL_GROUP_REPETITIONS 30



enum LOCAL_SEARCH_CRITERIA{BEST_IMPROVEMENT, FIRST_IMPROVEMENT}; 


//<! This class is resposible by reading and storing all the input data 
// We will use a .txt file as input in this code and supose that all processing 
// times are equal in different machines. 
class Input{

    private:
        bool successfull;                           ///< This variable is true if the input reading was successfull
        string fileName;                            ///< Input file name
        int numberOfMachines;                       ///< Number of machines available 
        int numberOfJobs;                           ///< Number of jobs to be scheduled 
        vector<double> maintenanceTimes;            ///< Maintenance time on each machine
        vector<vector<double> > processingTimes;    ///< Processing time of each job in each machine
        vector<vector<double> > deteriorationFactors;  ///< Deterioration factor of each job in each machine 

    public:  
        Input(){ successfull = false; }

        /**
         * Member constructor
         **/
        Input(string fileName){
            successfull = false; 
            this->fileName = fileName; 
        }

        /**
         * Read a input file with a given name
         * */
        void readInput(string file){
            cout<<file<<endl;
            ifstream arq(file.c_str());

            if(!arq.good()){
                cerr<<"Input file not found.\n"; 
                successfull = false;

            }

            arq>>numberOfMachines;
            arq>>numberOfJobs;

            #ifdef DEBUG
            cout<<numberOfMachines<<" "<<numberOfJobs<<endl;
            #endif 
            maintenanceTimes.resize(numberOfMachines);
            processingTimes.resize(numberOfMachines);
            deteriorationFactors.resize(numberOfMachines);

            //In the current version of the problem the jobs have the same processing time in all machines 
            //so you just need to read the processing time once and then replicate it to all machines 
            double pt;
            for(int i=0;i<numberOfJobs;i++){
                arq>>pt;
                for(int j=0; j<numberOfMachines;j++)
                    processingTimes[j].push_back(pt);
            }
            
            
            for(int i=0; i<numberOfMachines;i++)
                arq>>maintenanceTimes[i];

            for(int i=0;i < numberOfMachines;i++){
                deteriorationFactors[i].resize(numberOfJobs); 
                for(int j=0; j<numberOfJobs;j++)
                    arq>>deteriorationFactors[i][j];
            }
            successfull = true;
        }

        /**
         * This function is just to debug the reading process. If the file was read right it will be print
         * right here 
         * */
        void printInput(){
            cout<< " --------------------  Starting printing the instance --------------------  "<<endl; 
            cout<<setw(30)<<"Number of machines:" << numberOfMachines <<"\n";
            cout<<setw(30)<<"Number of jobs:" << numberOfJobs <<"\n";
            
            cout<<"Maintenance times"<<endl; 
            for(int i=0; i<maintenanceTimes.size(); i++)
                cout<<maintenanceTimes[i]<<" "; 
            
            cout<<endl;

            //In this version of the problem the jobs have the same processing time in all the machines 
            cout<<"Processing times"<<endl; 
            for(int i=0; i < numberOfJobs; i++)
                cout<<processingTimes[0][i]<<" "; 
            
            cout<<endl;

            cout<<"Deterioration factors"<<endl;
            for(int i=0; i<numberOfMachines; i++){
                cout<<"Machine "<<i<<endl;
                for(int j=0; j<numberOfJobs;j++)
                    cout<<deteriorationFactors[i][j]<<" ";
                cout<<endl; 
            }

            cout<< " --------------------  End printing the instance --------------------  "<<endl; 
        }


        /**
         * Returns true if the input file was read successfully or false otherwise 
         * */
        bool readSuccessfull(){
            return successfull; 
        }


        /**
         * Start the input file reading (if the file name was already informed) 
         * */
        void readInput(){
            if(this->fileName.empty())
                throw("No file name given");

            readInput(this->fileName);   
            
        }

        /**
         * Get the maintenance time for all machines
         * */
        vector<double> getMaintenanceTimes(){ 
            return maintenanceTimes; 
        }


        /***
         * Get maintenance time of a single machine  
         * */
        double getMaintenanceTimes(int machine){ 
            if(machine < 0 || machine >= numberOfMachines) 
                assert("Invalid machine index");

            return maintenanceTimes[machine]; 
        }

        vector<vector<double> > getDeteriorationFactors() { return deteriorationFactors; }  

        vector<double> getDeteriorationFactors(int machine){
            if(machine < 0 || machine >= numberOfMachines) 
                assert("Invalid machine index");
            return deteriorationFactors[machine];
        }

        vector<vector<double> > getProcessingTimes(){ return processingTimes; }

        vector<double> getProcessingTimes(int machine){
            if(machine < 0 || machine >= numberOfMachines) 
                assert("Invalid machine index");
            
            return processingTimes[machine];
        }
        
        void setProcessingTimes(vector<vector<double> > times) {  processingTimes = times; } 
        void setDeteriorationFactors (vector<vector<double> > factors){ deteriorationFactors = factors; }
        void setMaintenanceTimes(vector<double> times){ maintenanceTimes = times; }

        int getNumberOfJobs(){ return numberOfJobs; }
        int getNumberOfMachines(){ return numberOfMachines; }
        
        void setNumberOfJobs(int value){  numberOfJobs = value; }
        void setNUmberOfMachines(int value) { numberOfMachines = value;}

        int bestFirstJobOnMachine(int machine, int jobA, int jobB ){
            if(jobA < 0 || jobA >= numberOfJobs || jobB < 0 || jobB >= numberOfJobs )
                assert("Invalid job indexes"); 
            
            if(machine < 0 || machine >= numberOfMachines)
                assert("Invalid machine indexes"); 

            double factorA  = deteriorationFactors[machine][jobA]; 
            double factorB  = deteriorationFactors[machine][jobB]; 
            double processA = processingTimes[machine][jobA];
            double processB = processingTimes[machine][jobB];

            if(processA + factorA*processB <= processB * factorB*processA)
                return jobA;
            else
                return jobB;
        }

}; 


class Util{

    public: 
        static bool jobSortCompartion(Input *input, int m, int i, int j ){
            if(input->bestFirstJobOnMachine(m,i,j) == i)
                return true; 
            else 
                return false; 
        }

        /***
         *  It performs a binary search to say what is the best position to put a job in a sequence 
         * */
        static int bestInsertionPosition(Input *input, int &machine, int &job, int first , int last , vector<int> &sequence){
            
            if(sequence.size()==0)
                return 0; 

            int firstJob = sequence[first]; 
            int lastJob = sequence[last]; 

            if(last < first){
                cout<<"ERROR | Invalid interval "<<endl;
                exit(0);
            }
                
            if(last - first == 1){
                if(input->bestFirstJobOnMachine(machine, job, firstJob) == job)
                    return first;
                else if(input->bestFirstJobOnMachine(machine, job, lastJob) == job)
                    return last;
                else 
                    return last + 1;
            }

            if(last  == first){
                if(input->bestFirstJobOnMachine(machine,job, firstJob) == job )
                    return first;
                else
                    return first + 1; 
            }

            int middle = (first + last)/2 ; 
            int middleJob = sequence[middle];

            if(input->bestFirstJobOnMachine(machine, middleJob, job) == job){
                return bestInsertionPosition(input, machine, job, first, middle, sequence); 
            }else{
                return bestInsertionPosition(input,machine, job, middle+1, last, sequence);
            }
        }
};



struct Group{ 
    vector<int> sequence; 
    int machine; 
    double cost; 
    double accumulatedDeteriorationFactor; 

    Group(){ 
        cost = 0; 
        machine = -1; 
        accumulatedDeteriorationFactor = 1; 
        sequence.clear();
    }

    void reset(){
        cost = -1; 
        machine = -1; 
        accumulatedDeteriorationFactor = 1; 
        sequence.clear();
    }

    /***
     * This function inserts the job with a given index in the job sequence.
     * No validation about job repetitions are done
     * */
    void insertJobAtEnd(Input *input, int job){
        
        if(job >= 0 && job < input->getNumberOfJobs()){
            double deterioration = input->getDeteriorationFactors(machine)[job];
            double processingTime = input->getProcessingTimes(machine)[job]; 
            addProcessingTimeAndDeterioration(cost, processingTime, accumulatedDeteriorationFactor, deterioration);
            sequence.push_back(job);

        }else
            assert("Group::InsertJobAtEnd  - Invalid job index");
        
    }

    void addProcessingTimeAndDeterioration(double &_cost,const double &processingTime, double &accumFactor, const double & factor){
        _cost += accumFactor*processingTime; 
        accumFactor *= factor; 
    }

    /**
     *  Removes a job at the end of a group sequence and recalculate the cost and deterioration factor
     *  Complexity O(1)
     * */
    void removeJobAtEnd(Input *input){
        int job = sequence.back(); 
        
        double deterioration = input->getDeteriorationFactors(machine)[job];
        double processingTime = input->getProcessingTimes(machine)[job]; 

        accumulatedDeteriorationFactor /= deterioration; 
        cost -= processingTime*accumulatedDeteriorationFactor; 

        sequence.erase(sequence.begin()+sequence.size()-1);
    }

    /**
     * Insert the job in a given position in the sequence. No evaluation about 
     * the optimality or feasibility of insertion is done here.  
     * */
    void insertJobAtPosition(Input *input, int job, int position){
        
        vector<int> newSequence; 
        double newCost = 0;
        double newDeterioration = 1; 
        
        vector<double> deteriorations = input->getDeteriorationFactors(machine);
        vector<double> processingTimes = input->getProcessingTimes(machine);

        if(job < 0 || job > input->getNumberOfJobs()){
            assert("Group::insertJobAtPosition - invalid job index");
            return; 
        }
        
        if(position >= 0 && position <= sequence.size()){
            if(position == sequence.size()){
                insertJobAtEnd(input,job);
           } else{
                sequence.insert(sequence.begin()+position, job);
                for(unsigned int i = 0; i < sequence.size(); i++){
                        addProcessingTimeAndDeterioration(newCost,processingTimes[sequence[i]], newDeterioration, deteriorations[sequence[i]] );
                }

                cost = newCost;
                accumulatedDeteriorationFactor = newDeterioration;
            } 

        }else
            assert("Group::insertJobAtPosition - invalid position index");
            
    }

    /**
     *  Removes a job from the group sequence
     *  It recalculate all the processing times and deteriorations. 
     *  Complexity O(n)
     * */
    void removeJobAt(Input *input, int jobPosition){
        vector<int> newSequence; 
        double newCost = 0; 
        double newDeterioration = 1; 

        vector<double> deteriorations = input->getDeteriorationFactors(machine);
        vector<double> processingTimes = input->getProcessingTimes(machine); 

        for(unsigned int i = 0; i < sequence.size(); i++){
            if(i != jobPosition){
                addProcessingTimeAndDeterioration(newCost, processingTimes[sequence[i]],newDeterioration,deteriorations[sequence[i]]);
                newSequence.push_back(sequence[i]); 
            }
        }

        cost = newCost;
        accumulatedDeteriorationFactor = newDeterioration;
        sequence = newSequence; 

    }

    /**
     * 
     * */
    bool canSplit(Input *input , int machine){
        double delay = 0; 
        double df = 1.0; 
        double actualServiceTime;  

        vector<double> detFactors = input->getDeteriorationFactors(machine); 
        vector<double> serviceTime = input->getProcessingTimes(machine); 
        double maintenanceTime = input->getMaintenanceTimes(machine); 

        for(int i=0; i < sequence.size(); i++){
            actualServiceTime = serviceTime[sequence[i]]*df; 
            df *= detFactors[sequence[i]]; 
            delay += actualServiceTime  - serviceTime[sequence[i]] ; 
        }

        return delay >= maintenanceTime; 
    }

}; 


class Solution{

    private: 
        map<int, vector<Group> > schedule; 
        vector<double> machineCosts; 
        double cost; 
        int makespanMachine; 

        /***
         * 
         * */
        double evaluateMaintenanceCost(Input *input,int  indexMachine){
            double maintenanceCost = 0; 
            
            if(indexMachine < 0 || indexMachine >= input->getNumberOfMachines())
                assert("Solution::evaluateMaintenanceCost - Invalid machine cost");
            if(this->schedule.find(indexMachine) == this->schedule.end())
                assert("Solution::evaluateMaintenanceCost - Machine not found"); 
            
            if(this->schedule[indexMachine].size() > 1 )
                maintenanceCost = (this->schedule[indexMachine].size()-1)*input->getMaintenanceTimes(indexMachine); 

            return maintenanceCost; 
        }

        /**
         * 
         * */
        double evaluateProcessingTimeCost(Input *input, int indexMachine){
            double totalCost  = 0; 

            if(indexMachine < 0 || indexMachine >= input->getNumberOfMachines())
                assert("Solution::evaluateProcessingTimesCost - Invalid machine cost");
            if(this->schedule.find(indexMachine) == this->schedule.end())
                assert("Solution::evaluateProcessingTimesCost - Machine not found"); 

            vector<Group> val = this->schedule[indexMachine]; 
            for(int i=0; i<val.size(); i++){
                double deterioration = 1; 
                double processingTime  = 0; 
                vector<double> deteriorationFactors = input->getDeteriorationFactors(indexMachine); 
                vector<double> times  = input->getProcessingTimes(indexMachine); 

                for(int j = 0; j< val[i].sequence.size(); j++){
                    processingTime = deterioration*times[val[i].sequence.size()];
                    deterioration *= deteriorationFactors[val[i].sequence.size()];  
                }

            }

            return totalCost; 
        }

        double evaluatePenalties(Input &input); 

    public: 
        /**
         * 
         * */
        Solution(){
            this->cost = -1e20; 
            this->makespanMachine = -1; 
        }

        /**
         * 
         * */
        Solution(int numMachines){
            
            machineCosts.resize(numMachines, 0);
            makespanMachine = -1; 
            cost = -1e20; 
        }

        /**
         * 
         * */
        Solution(const Solution &other){
            this->cost = other.cost; 
            this->makespanMachine = other.makespanMachine;
            this->machineCosts = other.machineCosts;

            for( auto const& [key, val] : other.schedule){
                this->schedule[key].resize(val.size()); 
                for(int i=0; i<val.size(); i++)
                    this->schedule[key][i] = val[i];
            }
        }

        /**
         * 
         * */
        Solution &operator=(Solution &other){
            this->cost = other.cost; 
            this->makespanMachine = other.makespanMachine;
            this->machineCosts = other.machineCosts;

            for( auto const& [key, val] : other.schedule){
                this->schedule[key].resize(val.size()); 
                for(int i=0; i<val.size(); i++)
                    this->schedule[key][i] = val[i];
            }

            return *this; 
        }

        /**
         * 
         * */
        bool operator<(Solution & b){
            return this->cost< b.cost; 
        }

        void initializeMachineCosts(int numMachines){
            this->machineCosts.resize(numMachines,0);
        }

        /**
         * 
         * */
        int getMakespanMachine(){
            return this->makespanMachine; 
        }


        /**
         * 
         * */
        double getMachineCost(const int indexMachine)const{

            if(indexMachine < 0 || indexMachine >= this->machineCosts.size())
                assert("Solution::getMachineCost - Invalid machine cost");
            
            return machineCosts[indexMachine]; 
        }

        /**
         * 
         * */
        vector<double> getMachineCosts(){
            return this->machineCosts; 
        }

        /**
         * 
         * */
        int getNumberOfGroups(){
            int sum = 0; 

            for( auto const& [key, val] : this->schedule)
                sum += val.size();

            return sum; 
        }

        /**
         * 
         * */
        int getNumberOfGroups(int indexMachine){
            if(this->schedule.find(indexMachine) != this->schedule.end() )
                return this->schedule[indexMachine].size();
            
            return 0;
        }

        /**
         * 
         * */
        bool isFeasible(int numJobs, int numMachines){
            vector<int> presentJobs;
            
            presentJobs.resize(numJobs); 

            for( auto const& [key, val] : this->schedule)
                for(unsigned int i=0; i< val.size(); i++){
                    for(unsigned int j=0; j< val[i].sequence.size();j++){
                        if(val[i].sequence[j] < 0  || val[i].sequence[j] >= numJobs)
                            return false; 
                    
                            presentJobs[ val [i].sequence[j] ]++; 
                        }
                    }
            /***
            for(int i=0; i<presentJobs.size(); i++){
                cout<<presentJobs[i]<<" ";
                if(i%10 == 9)
                    cout<<endl;
            }
            cout<<endl; 
            ***/

            for(int i=0; i<presentJobs.size(); i++)
                if(presentJobs[i] != 1)
                    return false;

           return true;
        }

        /**
         * 
         * */
        set<int> getMissingJobs(int numJobs){
            vector<bool> presentJobs;
            presentJobs.resize(numJobs,false);

            for(const auto &[key,val]: this->schedule)
                for(int i=0;i<val.size();i++)
                    for(int j=0; j<val[i].sequence.size();j++)
                        presentJobs[val[i].sequence[j]] = true;
            
            set<int> returned;
            for(int i=0;i<numJobs;i++) 
                if(!presentJobs[i]) 
                    returned.insert(i);

            return returned;
        }

        /**
         * 
         * */
        bool isEmpty(){

            bool empty = true;
            
            if(this->schedule.size() > 0){
                for(const auto &[ key, val] : this->schedule){
                    if(val.size()> 0 && any_of(val.begin(), val.end(), [](Group v){ return v.sequence.size() >0; })){
                        empty = false; 
                        break; 
                     }
                }
            } 

            
            return empty; 
        }

        /**
         * 
         * */
        bool hasMachine(int indexMachine){
            return this->schedule.find(indexMachine) != this->schedule.end();
        }

        /**
         * 
         * */
        void setCost(double value){ this->cost = value; }  

        /**
         * 
         * */
        double getCost(){ return this->cost; }  
        
        /**
         * 
         * */
        void updateCost(bool usePenalty = true);

        /**
         * 
         * */
        map<int, vector<Group> > getSchedule(){
            return this->schedule; 
        }

        /**
         * 
         * */
        vector<Group> getSchedule(int indexMachine){
            if(indexMachine < 0 || indexMachine > this->schedule.size())
                assert("Solution::getSchedule - Index machine not valid"); 

            return this->schedule[indexMachine]; 
        }

        /**
         * 
         * */
        void setSchedule(map<int, vector<Group> > newSchedule, const vector<double> &maintenanceTimes){
            this->schedule = newSchedule; 
            this->machineCosts.resize(newSchedule.size()); 

            int longestMachine = 0; 
            double longestTime = 0; 

            for(const auto& [key, val] : this->schedule){
                int numGroups = val.size(); 
                double maintenanceTime  = numGroups <= 1 ? 0 : (numGroups-1)*maintenanceTimes[key]; 
                double totalProcessingTime =0; 

                for(int i=0; i < val.size(); i++){
                    totalProcessingTime += val[i].cost; 
                }
                this->machineCosts[key] = totalProcessingTime + maintenanceTime; 

                if(this->machineCosts[key] > longestTime){
                    longestTime = this->machineCosts[key];
                    longestMachine = key; 
                }    
            }

            this->cost  = longestTime; 
            this->makespanMachine  = longestMachine; 

        }

        /**
         * 
         * */
        Group & getScheduleGroup(int machine, int indexGroup){
            if(!this->hasMachine(machine))
                assert("Invalid machine");
            
            if( indexGroup < 0 || indexGroup >=  this->schedule[machine].size() )
                assert("Invalid group index"); 

            return this->schedule[machine][indexGroup]; 
        } 

        /**
         * 
         * */
        void removeGroup(int indexMachine, int indexGroup,double maintenanceTime){
            if(this->schedule.find(indexMachine) == this->schedule.end()){
                assert("Solution::removeGroup - index machine not found");
                return;
            }

            if(this->schedule[indexMachine].size() <= indexGroup || indexGroup < 0)
                assert("Solution::removeGroup - invalid index group");

            vector<Group>::iterator vec_it = this->schedule[indexMachine].begin();
            advance(vec_it, indexGroup);                                           //get vector element
            double groupCost = vec_it->cost; 
            this->schedule[indexMachine].erase(vec_it);                            // Remove group from machine schedule

            if(this->schedule[indexMachine].size() >0)
                this->machineCosts[indexMachine] -= maintenanceTime; 
            this->machineCosts[indexMachine] -= groupCost;

            double newCost = 0; 
            int newMakespanMachine = 0; 

            for(int i=0;i<machineCosts.size(); i++){
                if(newCost < machineCosts[i]){
                    newCost = machineCosts[i];
                    newMakespanMachine = i; 
                }
            }
            cost = newCost; 
            makespanMachine = newMakespanMachine; 
        }

        /**
         * 
         * */
        void updateSolutionWithGroupInsert(vector<Group> newGroups, int indexMachine, double maintenanceTime) {
            
            double totalMaintenanceTime = 0; 

            if( this->schedule.find(indexMachine) == this->schedule.end()){
                
                double _cost = max((unsigned long)0, newGroups.size()-1)*maintenanceTime; 
                for(int i=0;i<newGroups.size();i++)
                    _cost += newGroups[i].cost;  
                
                this->machineCosts[indexMachine] = _cost;
                if(_cost > cost ){
                    cost = _cost; 
                    makespanMachine = indexMachine;
                } 

                for(int i=0;i<newGroups.size(); i++)
                    this->schedule[indexMachine].push_back(newGroups[i]);

            }else{
                double groupCost = 0; 
                double newMaintenanceCosts = this->schedule[indexMachine].size() == 0? -1*maintenanceTime : 0 ;

                for(int i=0;i<newGroups.size();i++){
                    this->schedule[indexMachine].push_back(newGroups[i]); 
                    groupCost += newGroups[i].cost; 
                    totalMaintenanceTime += maintenanceTime;
                }

                if(newGroups.size() > 0)
                    machineCosts[indexMachine] += groupCost + totalMaintenanceTime; 

                if(indexMachine == this->makespanMachine) 
                    cost = machineCosts[indexMachine];
                else if(machineCosts[indexMachine] > cost){
                    this->makespanMachine = indexMachine; 
                    cost = machineCosts[indexMachine];
                }
            }
        }

        /**
         * 
         * */
        int getLongestGroupIndexOnMachine(int machineIndex){
           
            if(this->schedule.find(machineIndex) == this->schedule.end()){
                assert("Solution::removeGroup - index machine not found");
                return -1;
            }

            int biggerGroup = 0; 
            double biggerCost = 0;

            int numGroups = this->schedule[machineIndex].size();
            for(int i=0; i <numGroups; i++){
                if(schedule[machineIndex][i].cost > biggerCost){
                    biggerGroup = i; 
                    biggerCost = this->schedule[machineIndex][i].cost;
                }
            }

            return biggerGroup; 
        }

        /***
         * 
         * */
        void updateGroupOnSchedule(int groupIndex, Group updatedGroup){

            int machineIndex = updatedGroup.machine; 
            Group oldGroup = this->schedule[machineIndex][groupIndex]; 

            double oldCost = this->schedule[machineIndex][groupIndex].cost;
            double newCost = updatedGroup.cost; 
            
            this->schedule[machineIndex][groupIndex] = updatedGroup; 
            machineCosts[machineIndex] += newCost - oldCost; 
            
            if(machineIndex == makespanMachine){
                if(newCost > oldCost){
                    cost = machineCosts[machineIndex]; 
                }else{ 
                    double maxCost = machineCosts[machineIndex]; 
                    int newMakespanMachine = makespanMachine; 

                    for(int i=0; i<machineCosts.size(); i++){
                        if(machineCosts[i] > maxCost ){
                            maxCost  = machineCosts[i];
                            newMakespanMachine = i; 
                        }
                    } 
                    cost = maxCost;
                    makespanMachine = newMakespanMachine; 
                }
            }else if(machineCosts[machineIndex] > cost){
                makespanMachine = machineIndex; 
                cost = machineCosts[machineIndex]; 
            } 

        }

        /**
         * 
         * */
        void printSolution(){
            
            cout<<"------   Solution description   ------"<<endl; 
            cout<<"Makespan: "<<this->cost<<endl;
            cout<<"Logest time machine: "<<this->makespanMachine<<endl; 
            for( auto const& [key, val] : this->schedule){
                cout<<"Machine : " <<key<<endl;
                cout<<"Machine cost: " <<machineCosts[key]<<endl;
                for(int i=0; i<val.size(); i++){
                    cout<<setw(40)<<"\tGroup "<<i << " of machine " << val[i].machine<<endl;
                    cout<<setw(40)<<"\tGroup cost: "<<val[i].cost; 
                    cout<<setw(40)<<"\tGroup final deterioration factor: "<<val[i].accumulatedDeteriorationFactor<<endl;  
                    cout<<"\t ***   Jobs sequence   ***   \t"; 
                    for(int j=0; j<val[i].sequence.size();j++)
                        cout<<setw(5)<<val[i].sequence[j]<<" ";
                    cout<<endl; 
                }   
            }
        }

        /**
         * 
         * */
        void printSolutionToFile(ofstream &file){
            if(!file.good())
                cerr<<"Problem with ofstream object\n"; 
            
            file<<"------   Solution description   ------"<<endl; 
            file<<"Makespan: "<<this->cost<<endl;
            file<<"Logest time machine: "<<this->makespanMachine<<endl; 
            for( auto const& [key, val] : this->schedule){
                file<<"Machine : " <<key<<endl;
                file<<"Machine cost: " <<machineCosts[key];
                for(int i=0; i<val.size(); i++){
                    file<<setw(40)<<"\tGroup "<<i<<endl;
                    file<<setw(40)<<"\tGroup cost: "<<val[i].cost; 
                    file<<setw(40)<<"\tGroup final deterioration factor: "<<val[i].accumulatedDeteriorationFactor<<endl;  
                    file<<"\t ***   Jobs sequence   ***   \t"; 
                    for(int j=0; j<val[i].sequence.size();j++)
                        file<<setw(5)<<val[i].sequence[j]<<" ";
                    file<<endl; 
                }   
            }
        }

        /**
         * 
         * */
        void printSolutionToFile(string fileName){
            ofstream arq;
            arq.open(fileName.c_str());

            if(arq.good())
                printSolutionToFile(arq);
            else
                assert("Error in opening file\n"); 

        }
};

/**
 * 
 * 
 * */
class NeighborhoodManager{

    private: 
       // Solution &solution;
        Input *input; 

        /**
         * This function will not be a neighborhood, but just a function that will correct
         * problems caused by other neighborhoods moves. An "optimal" strategy to divide groups
         * is not know (yet)
         * */
        vector<Group> splitGroup(Group &original, int machine)const {
            
            vector<Group> groups; 
            double accumulatedTime =0;
            double accumulatedDeterioration = 1.0; 
            double accumulatedDelay =0; 
            
            Group currentGroup; 
            double maintenanceTime = input->getMaintenanceTimes(machine); 
            vector<double> deteriorations = input->getDeteriorationFactors(machine); 
            
            vector<double> processingTimes  = input->getProcessingTimes(machine);
            
            for(int i= 0; i<original.sequence.size(); i++){
                int job = original.sequence[i];
                accumulatedTime += processingTimes[job]*accumulatedDeterioration; 
                accumulatedDelay += processingTimes[job]*(accumulatedDeterioration-1); 
                accumulatedDeterioration *= deteriorations[job]; 

                if(accumulatedDelay > maintenanceTime){
                    
                    groups.push_back(currentGroup);
                    currentGroup.reset(); 

                    currentGroup.machine = machine; 
                    accumulatedDelay =0; 
                    accumulatedDeterioration = 1.0; 
                    accumulatedTime =processingTimes[job]*accumulatedDeterioration;
                }
                
                currentGroup.sequence.push_back(job);
                currentGroup.cost = accumulatedTime;
                currentGroup.accumulatedDeteriorationFactor = accumulatedDeterioration; 
            }
            return groups; 
        }

        /**
         * Update information of a group after it changes from a machine to another
         * */
        vector<Group> updateGroupInfo(Group &group, vector<double> &deteriorations, 
                                    vector<double> &processingTimes,  double maintenanceTime )const {
            
            vector<Group> groupsReturned; 
            
             //Evaluate cost group
            double cost = 0;   
            double accumulatedDeterioration = 1;
            double totalProcessingTime = 0; 
            for(int i= 0; i< group.sequence.size(); i++){
                cost += accumulatedDeterioration*processingTimes[group.sequence[i] ]; 
                totalProcessingTime +=  processingTimes[ group.sequence[i] ];
                accumulatedDeterioration *= deteriorations[ group.sequence[i] ];
            }
            
            groupsReturned.push_back(group); 
            
            //Eventually the group need to be splitted in the new machine, because the total delay
            // is greater than the maintenance time. This change is done bellow 
            if(cost - totalProcessingTime > maintenanceTime){
                vector<Group> newGroups = splitGroup(group, group.machine); 
            }
            
            return groupsReturned; 
        }

    public: 

        /**
         * 
         * */
        NeighborhoodManager(Input * input){
           this->input = input;
        }

        /**
         * 
         * */
        Solution * swapGroupMachine(Solution &solution, int machineGroupA, int indexGroupA, int machineGroupB, int &indexGroupB){

            Solution *returnedSolution = new Solution(solution);
        
            Group groupA = solution.getScheduleGroup(machineGroupA, indexGroupA);
            Group groupB = solution.getScheduleGroup(machineGroupB, indexGroupB); 

           vector<double> deteriorationsMachineA = input->getDeteriorationFactors(machineGroupA);
            vector<double> deteriorationsMachineB = input->getDeteriorationFactors(machineGroupB); 

            vector<double> processingTimesMachineA = input->getProcessingTimes(machineGroupA);
            vector<double> processingTimesMachineB = input->getProcessingTimes(machineGroupB);
            
            double maintenanceMachineA = input->getMaintenanceTimes(machineGroupA);  
            double maintenanceMachineB = input->getMaintenanceTimes(machineGroupB); 

            //Make the swap of machines, evaluate the new costs and update information
            groupA.machine = machineGroupB; 
            groupB.machine = machineGroupA; 
            vector<Group> newGroupsB = updateGroupInfo(groupA, deteriorationsMachineB, processingTimesMachineB, maintenanceMachineB);
            vector<Group> newGroupsA = updateGroupInfo(groupB, deteriorationsMachineA, processingTimesMachineA, maintenanceMachineA);

            vector<double> machineCosts = returnedSolution->getMachineCosts(); 

            returnedSolution->removeGroup(machineGroupA,indexGroupA,input->getMaintenanceTimes(machineGroupA)); 
            returnedSolution->removeGroup(machineGroupB,indexGroupB,input->getMaintenanceTimes(machineGroupB)); 
            returnedSolution->updateSolutionWithGroupInsert(newGroupsB, groupA.machine, maintenanceMachineB);
            returnedSolution->updateSolutionWithGroupInsert(newGroupsA, groupB.machine, maintenanceMachineA);

            machineCosts = returnedSolution->getMachineCosts(); 
            return returnedSolution; 
            
        }

        /**
         * 
         * */
        Solution * swapGroupJob(const Solution &solution, Group &groupA, int jobA, Group &groupB, int jobB){
                Solution *returnedSolution = new Solution(solution);
                if(jobA < 0 || jobA >= groupA.sequence.size())
                    assert("NeighborhoodManager::swapGroupJob - Invalid job index");
                if(jobB < 0 || jobB >= groupB.sequence.size())
                    assert("NeighborhoodManager::swapGroupJob - Invalid job index");

                int oldCostA = groupA.cost; 
                int oldCostB = groupB.cost; 

                return returnedSolution; 
        }

        /**
         * 
         * */
        Solution * groupInsertJob(Solution &solution, int indexMachine, int indexGroup , int job){
            Solution *returnedSolution = new Solution(solution);
            
            if(job < 0 || job >= this->input->getNumberOfJobs())
                assert("NeighborhoodManager::groupInsertJob - Job index not valid"); 

            if(!returnedSolution->hasMachine(indexMachine))
                    assert("NeighborhoodManager::groupInsertJob - Machine index not found");
            
            if(indexGroup < 0 || indexGroup >= returnedSolution->getSchedule(indexMachine).size() )
                    assert("NeighborhoodManager::groupInsertJob - The group index is invalid");
            
            map<int, vector<Group> > schedule = returnedSolution->getSchedule(); 
           
            Group modifiedGroup = schedule[indexMachine][indexGroup]; 
            vector<int> sequence = modifiedGroup.sequence; 
            int bestPosition = sequence.size()==0? 0 : Util::bestInsertionPosition(input, indexMachine, job,0 , sequence.size()-1 , sequence);
            
            modifiedGroup.insertJobAtPosition(input,job,bestPosition);         
            returnedSolution->updateGroupOnSchedule(indexGroup,modifiedGroup); 

            return returnedSolution; 
        }

        /**
         * 
         * */
        Solution * removeJob(const Solution * solution, int indexMachine,  int groupIndex, int jobIndex){
                Solution *returnedSolution = new Solution(*solution); 

                if(!returnedSolution->hasMachine(indexMachine))
                    assert("NeighborhoodManager::removeJob - Machine index not found");
                
                if(returnedSolution->getSchedule(indexMachine).size() == 0)
                    assert("NeighborhoodManager::removeJob - This machine has no jobs in it");

                if(groupIndex < 0 || groupIndex >= returnedSolution->getSchedule(indexMachine).size() )
                    assert("NeighborhoodManager::removeJob - The group index is invalid");

                map<int, vector<Group> > schedule = returnedSolution->getSchedule();
                Group modifiedGroup = schedule[indexMachine][groupIndex]; 

                modifiedGroup.removeJobAt(input, jobIndex); 
                if(modifiedGroup.sequence.size()>0)
                    returnedSolution->updateGroupOnSchedule(groupIndex, modifiedGroup); 
                else
                    returnedSolution->removeGroup(modifiedGroup.machine, groupIndex, input->getMaintenanceTimes(modifiedGroup.machine));

                return returnedSolution; 
        }

        /**
         * 
         * */
        Solution * changeJobGroup(Solution * solution, int jobIndex,int firstMachine,int firstGroup,int secondMachine,int secondGroup){
            
            map<int, vector<Group> > schedule = solution->getSchedule(); 
            int job = schedule[firstMachine][firstGroup].sequence[jobIndex]; 
            
            Solution *returnedSolution = removeJob(solution, firstMachine, firstGroup, jobIndex);
            schedule = returnedSolution->getSchedule();

            if(secondGroup == schedule[secondMachine].size() && secondGroup >0)
                secondGroup--;

            Solution *aux = returnedSolution;
            returnedSolution = groupInsertJob(*returnedSolution, secondMachine, secondGroup, job); 

            delete aux; 

            return returnedSolution; 
        }

        /**
         * 
         * */
        Solution * deleteGroup(const Solution &solution, int groupIndex, int indexMachine)const {
            Solution *returnedSolution = new Solution(solution);
            map<int,vector<Group> > schedule  = returnedSolution->getSchedule();

            if(schedule.find(indexMachine) != schedule.end() && groupIndex >= 0 && groupIndex < schedule[indexMachine].size()){
                returnedSolution->removeGroup(indexMachine,groupIndex,input->getMaintenanceTimes(indexMachine)); 
            }else{
                assert("NeighborhoodStructure::deleteGroup - Machine or groud not found");
            }
            return returnedSolution; 
        } 

        /***
         *  Insert a group in the schedule of a machine
         * */
        Solution * insertGroup(const Solution &solution, Group& group, int indexMachine)const{
            
            group.machine = indexMachine;
            Solution * returnedSolution = new Solution(solution); 
            vector<double> deteriorations = input->getDeteriorationFactors(indexMachine);
            vector<double> processingTimes = input->getProcessingTimes(indexMachine);
            double maintenanceTime = input->getMaintenanceTimes(indexMachine); 
            
            vector<Group> newGroups = updateGroupInfo(group, deteriorations,processingTimes, maintenanceTime);

            returnedSolution->updateSolutionWithGroupInsert(newGroups, indexMachine, maintenanceTime); 

            return returnedSolution; 
        } 
};


class MainHeuristic {

    private:
        Input *input; 
        Solution bestSolution; 
        Solution currentSolution; 

        vector<int> linearizeSchedule(map<int, vector<Group> > schedule){
            vector<int>returned; 
            for(const auto &[key, value] : schedule){
                returned.push_back(value.size()); 
            }
            return returned; 
        }

        /**
         * 
         * */
        void testNewSolution(Solution *test,double initialCost,int &withoutImprovement,bool &improved, int maxTries){
                withoutImprovement++; 
                if(test->getCost() < initialCost)
                    improved = true; 
                if(withoutImprovement == maxTries)
                    return; 
                
        }

        /**
         * 
         * */
        void CreateInitialSolution(int strategy = 1){

            switch(strategy){

                case 1:
                    cheapestGroup(); 
                    break;
                case 2:
                    cheapestMachineFirst();
                    break;
                case 3:
                    //strategy 3
                    break;
                case 4:
                    //strategy 4
                    break; 
                default:
                    cerr<<"Solution strategy initialization not found\n"; 
                    break;
            }
        }

        /**
         * 
         * */
        bool getRandomMachineAndGroupWithJobs(int &machine, int &groupIndex, map<int, vector<Group> > schedule, int maxTriesMachine, int maxTriesGroup){
                int numRandTries = 0; 
               
                machine = rand()%schedule.size(); 
                while(schedule[machine].size() == 0  && numRandTries++ < maxTriesMachine)
                    machine = rand()%schedule.size(); 
               
                if(numRandTries >= maxTriesMachine)
                    return false; 

                numRandTries = 0; 
               
                groupIndex = rand()%schedule[machine].size(); 
                while(schedule[machine][groupIndex].sequence.size() == 0 && numRandTries++ <= maxTriesGroup)
                    groupIndex = rand()%schedule[machine].size(); 
               
                if(numRandTries >= maxTriesGroup)
                    return false;

                return true;  
        }


        /***
         * 
         * */
        void insertIntoBestGroup(Solution &solution, int job, int machine){

            vector<Group> scheduleOnMachine = solution.getSchedule(machine);
            double lowerCost = 1e100; 
            Group bestGroup; 
            int bestGroupIndex = 0;
            int bestPosition = 0; 
            
            for(int i=0; i<scheduleOnMachine.size();i++){
                Group group = scheduleOnMachine[i]; 
                double currentCost = group.cost;
                
                int position = Util::bestInsertionPosition(input, machine, job, 0 , group.sequence.size()-1 , group.sequence); 
                group.insertJobAtPosition(input,job,position); 

                if(group.cost - currentCost < lowerCost){
                    lowerCost = group.cost - currentCost; 
                    bestGroup = group;
                    bestPosition = position;  
                    bestGroupIndex = i; 
                }
            }

            if(scheduleOnMachine.size() == 0){
                Group newGroup;
                newGroup.machine = machine;
                newGroup.insertJobAtEnd(input, job);
                insertGroupOnCurrentSolution(newGroup);
            }else{
                scheduleOnMachine[bestGroupIndex].insertJobAtPosition(input, job, bestPosition); 
                solution.updateGroupOnSchedule(bestGroupIndex,scheduleOnMachine[bestGroupIndex]); 
            }
            
        }

        /***
         * 
         * */
        void cheapestMachineFirst(){

            vector<double> machineCost; 
            machineCost.resize(input->getNumberOfMachines(), 0);
             
            int numJobs = input->getNumberOfJobs(); 

            for(int i=0; i<numJobs; i++){
                double minCost = 1e50; 
                int cheapestMachine  = 0; 

                for(int j=0; j<machineCost.size(); j++){
                    if(machineCost[j] < minCost ){
                        minCost = machineCost[j];
                        cheapestMachine = j; 
                    }
                }
                insertIntoBestGroup(currentSolution, i, cheapestMachine); 
            }
            
        }

        /***
         * 
         * */
        void findCheapestGroup(vector<Group> groups, int job, int & bestGroup, int & bestPosition){
            double lowestCost = 1e20;

            for(int j=0;j<groups.size();j++){
                Group aux = groups[j]; 
                vector<int> sequence = aux.sequence; 
                int machine = aux.machine; 
                int position = sequence.size() == 0 ? 0 : Util::bestInsertionPosition(input, machine, job, 0, sequence.size()-1, sequence); 
                aux.insertJobAtPosition(input, job, position); 
                double newCost = aux.cost; 

                if(newCost < lowestCost){
                    lowestCost = newCost;
                    bestGroup = j; 
                    bestPosition = position; 
                }
            }
        }

        /**
         * 
         * */
        void insertGroupOnCurrentSolution(Group group){
            int maintenanceTime = input->getMaintenanceTimes(group.machine); 
            vector<Group> unitGroup; 
            unitGroup.push_back(group);  
            this->currentSolution.updateSolutionWithGroupInsert(unitGroup, group.machine, maintenanceTime);    
        }

        /***
         * 
         * */
        void cheapestGroup(){
            
            vector<Group> groups; 
            groups.resize(input->getNumberOfMachines()); 
            
            for(int i=0;i<groups.size();i++){
                groups[i].machine = i; 
                groups[i].cost = 0; 
            }
            
            for(int i=0;i<input->getNumberOfJobs(); i++){
                int bestGroup; 
                int bestPosition; 
               
                findCheapestGroup(groups,i, bestGroup,bestPosition);
                groups[bestGroup].insertJobAtPosition(input, i, bestPosition); 
                int machine = groups[bestGroup].machine;
               
                if(groups[bestGroup].canSplit(input, machine)){
                    insertGroupOnCurrentSolution(groups[bestGroup]);
                    groups.erase(groups.begin()+bestGroup);
                    Group newGroup;
                    newGroup.machine = machine;
                    newGroup.cost = 0;
                    groups.push_back(newGroup);
                }
               
            }

            for(int i=0; i<groups.size(); i++){
                int machine = groups[i].machine;
                insertGroupOnCurrentSolution(groups[i]); 
            }
        }

        /**
         * 
         * */
        void executeLocalSearch(){
            int nj = input->getNumberOfJobs();
            int nm = input->getNumberOfMachines(); 

            //First neighborhood
            for(int i=0; i<NUM_SWAP_GROUPS_LOCAL_SEARCH_REPETITIONS; i++)
                swapGroupsLocalSearch();
           
            #ifdef DEBUG
            cout<<"Current solution after A: "<<currentSolution.getCost()<<endl;
            #endif 
            
            if(currentSolution.getCost() < bestSolution.getCost())
                bestSolution = currentSolution; 

            //Second neighborhood
             for(int i=0; i< NUM_MOVE_JOB_TO_OTHER_INTERNAL_GROUP_REPETITIONS; i++)
                moveJobToOtherInternalGroup(); 

             #ifdef DEBUG
            cout<<"Current solution after B: "<<currentSolution.getCost()<<endl;
            #endif
            
            if(currentSolution.getCost() < bestSolution.getCost())
                bestSolution = currentSolution; 

            //Third neighborhood
            for(int i=0; i < NUM_SWAP_JOBS_LOCAL_SEARCH_REPETITIONS;i++)
                swapJobsLocalSearch();
            
            #ifdef DEBUG
            cout<<"Current solution after C: "<<currentSolution.getCost()<<endl;
            #endif
            
            if(currentSolution.getCost() < bestSolution.getCost())
                bestSolution = currentSolution; 

            //Fourth neighborhood
            for(int i=0; i<NUM_SWAP_GROUPS_LOCAL_SEARCH_REPETITIONS; i++)
                moveGroupLocalSearch(); 

            #ifdef DEBUG
            cout<<"Current solution after D: "<<currentSolution.getCost()<<endl;
            #endif
                
            if(currentSolution.getCost() < bestSolution.getCost())
                bestSolution = currentSolution;

            //Fifth neighborhood
            for(int i=0; i< NUM_MOVE_JOB_TO_OTHER_EXTERNAL_GROUP_REPETITIONS; i++)
                moveJobToOtherExternalGroup(); 
   
            #ifdef DEBUG
            cout<<"Current solution after E: "<<currentSolution.getCost()<<endl;
            #endif
            
        //    if(!currentSolution.isFeasible(nj,nm))
        //        cout<<"NFeasible\n";

            if(currentSolution.getCost() < bestSolution.getCost())
                bestSolution = currentSolution;
        }

        /**
         * 
         * */
        void moveJobToOtherInternalGroup(){

            double initialCost = this->currentSolution.getCost(); 
            map<int, vector<Group> > schedule = this->currentSolution.getSchedule(); 
            NeighborhoodManager manager(input); 

            bool improved = false; 
            int withoutImprovement = 0 ; 
            Solution *test = NULL; 

            while(!improved && withoutImprovement < MAX_MOVE_JOBS_ATTEMPTS*schedule.size()){

                int machine, firstGroupIndex; 
                if(! getRandomMachineAndGroupWithJobs(machine,firstGroupIndex, schedule, schedule.size(), 2*schedule.size()))
                    break; 

                int numRandTries = 0;
                int secondGroupIndex = rand()%schedule[machine].size();
                int maxTries = 2*schedule[machine].size(); 


                while(schedule[machine][secondGroupIndex].sequence.size() == 1 && firstGroupIndex == secondGroupIndex && numRandTries++ <= maxTries)
                    secondGroupIndex = rand()%schedule[machine].size(); 
                
                if(numRandTries >= maxTries )
                    break; 
                
                int job =  rand()%schedule[machine][firstGroupIndex].sequence.size(); 
                test = manager.changeJobGroup(&this->currentSolution, job, machine, firstGroupIndex,machine,secondGroupIndex);
                testNewSolution(test, initialCost, withoutImprovement, improved, MAX_MOVE_JOBS_ATTEMPTS*schedule.size()); 
                if(!improved)
                    delete test;
            }

            if(improved)
                this->currentSolution = *test; 
        }



        /**
         * 
         * */
        void moveJobToOtherExternalGroup(){

            double initialCost = this->currentSolution.getCost(); 
            map<int, vector<Group> > schedule = this->currentSolution.getSchedule(); 
            NeighborhoodManager manager(input); 

            bool improved = false; 
            int withoutImprovement = 0 ; 
            Solution *test = NULL; 
            
            while(!improved && withoutImprovement < MAX_MOVE_JOBS_ATTEMPTS*schedule.size()){
                int numRandTries =0;
                int firstMachine, firstGroupIndex, secondMachine, secondGroupIndex; 

                if(! getRandomMachineAndGroupWithJobs(firstMachine,firstGroupIndex, schedule, schedule.size(), 2*schedule.size()) )
                    break; 
                
                if(! getRandomMachineAndGroupWithJobs(secondMachine,secondGroupIndex, schedule, schedule.size(), 2*schedule.size() ) )
                    break;  

            
                int job =  rand()%schedule[firstMachine][firstGroupIndex].sequence.size(); 
                
                test = manager.changeJobGroup(&this->currentSolution, job, firstMachine, firstGroupIndex,secondMachine,secondGroupIndex);
                testNewSolution(test, initialCost, withoutImprovement, improved, MAX_MOVE_JOBS_ATTEMPTS*schedule.size());
                if(!improved)
                    delete test; 
            }

            if(improved)
                this->currentSolution = *test; 
        }

        /**
         * 
         * */
        void swapJobsLocalSearch(){

            double initialCost = this->currentSolution.getCost(); 
            map<int, vector<Group> > schedule = this->currentSolution.getSchedule(); 
            NeighborhoodManager manager(input); 

            bool improved = false; 
            int withoutImprovement = 0 ; 
            Solution *test = NULL; 

            while(!improved && withoutImprovement < MAX_SWAPS_FACTOR_ATTEMPTS*schedule.size() ){
                int chosenMachine = rand()%schedule.size(); 
                int numTries = 0; 
                int firstGroupIndex, secondGroupIndex; 
                
                //Try to pick a machine with two or more groups 
                while(schedule[chosenMachine].size() <= 1 && numTries++ < 50 )
                    chosenMachine = rand()%schedule.size(); 
                if(numTries >= 50)
                    break; 

                //picks to groups in the machine
                firstGroupIndex = rand()%schedule[chosenMachine].size(); 
                secondGroupIndex = rand()%schedule[chosenMachine].size(); 
                int countTries =0; 
                while(schedule[chosenMachine][firstGroupIndex].sequence.size() < 1){
                    firstGroupIndex = rand()%schedule[chosenMachine].size();
                    if(countTries++ > 50)
                        break;
                }
                
                if(countTries > 50){
                    withoutImprovement++;
                    continue;
                }
                
                countTries =0;
                while(firstGroupIndex == secondGroupIndex || schedule[chosenMachine][secondGroupIndex].sequence.size() <1){
                    secondGroupIndex = rand()%schedule[chosenMachine].size(); 
                    if(countTries++ > 50)
                        break; 
                }
                
                if(countTries > 50){
                    withoutImprovement++;
                    continue;
                }
                
                Group firstGroup = schedule[chosenMachine][firstGroupIndex]; 
                Group secondGroup = schedule[chosenMachine][secondGroupIndex]; 

                int firstJob = rand()%firstGroup.sequence.size(); 
                int secondJob = rand()%secondGroup.sequence.size(); 
                
                test =  manager.swapGroupJob(this->currentSolution,firstGroup, firstJob, secondGroup, secondJob); 
                testNewSolution(test, initialCost, withoutImprovement, improved, MAX_SWAPS_FACTOR_ATTEMPTS*schedule.size()); 
                if(!improved)
                    delete test; 

            }

            if(improved)
                this->currentSolution = *test; 
        }



        void moveGroupLocalSearch(){
            Solution *test = NULL; 
            double initialCost = currentSolution.getCost(); 
            vector<double> machineCosts = currentSolution.getMachineCosts(); 
            int makespanMachine = currentSolution.getMakespanMachine(); 
            map<int, vector<Group> > schedule = currentSolution.getSchedule();
            NeighborhoodManager manager(input); 

            if(schedule[makespanMachine].size() == 0 || schedule.size() == 1)
                return; 
            
            bool improved = false;
            int longestGroup = currentSolution.getLongestGroupIndexOnMachine(makespanMachine); 
         
            int withoutImprovement = 0;
            while(!improved && withoutImprovement <= MAX_SWAPS_FACTOR_ATTEMPTS*schedule.size()){
                Group group = schedule[makespanMachine][longestGroup]; 
                test = manager.deleteGroup(this->currentSolution, longestGroup, makespanMachine); 
                Solution *aux = test; 

                double smallerValue = initialCost; 
                double smallerMachine = makespanMachine; 
                for(int i=0; i<machineCosts.size();i++){
                    if(machineCosts[i] < smallerValue){
                        smallerValue = machineCosts[i];
                        smallerMachine = i; 
                    }

                }

                group.machine = smallerMachine;
                test = manager.insertGroup(*aux, group, smallerMachine); 
                delete aux; 

                testNewSolution(test, initialCost, withoutImprovement, improved, MAX_SWAPS_FACTOR_ATTEMPTS*schedule.size());
                
                if(!improved)
                    delete test; 
            }
            if(improved)
                this->currentSolution = *test; 
        }

        /**
         * 
         * */ 
        void swapGroupsLocalSearch(){

            Solution *test = NULL; 
            double initialCost = currentSolution.getCost(); 
            vector<double> machineCosts = currentSolution.getMachineCosts(); 
            int makespanMachine = currentSolution.getMakespanMachine(); 
            map<int, vector<Group> > schedule = currentSolution.getSchedule();
            NeighborhoodManager manager(input); 

            if(schedule[makespanMachine].size() == 0 || schedule.size() == 1)
                return; 
            
            bool improved = false;
            int longestGroup = currentSolution.getLongestGroupIndexOnMachine(makespanMachine); 

            
            int withoutImprovement = 0;
            while(!improved && withoutImprovement <= MAX_SWAPS_FACTOR_ATTEMPTS*schedule.size()){
            
                int chosenMachine = rand()%schedule.size(); 
                int count =0; 
            
                while((chosenMachine == makespanMachine || schedule[chosenMachine].size() == 0) && count < 5*schedule.size())
                    chosenMachine = rand()%schedule.size(); 
            
                int chosenGroup = rand()%schedule[chosenMachine].size(); 
            
                if(count == 5*schedule.size())
                    return; 

                test = manager.swapGroupMachine(currentSolution, makespanMachine, longestGroup,  chosenMachine,  chosenGroup);
                schedule = test->getSchedule();
                testNewSolution(test, initialCost, withoutImprovement, improved, MAX_SWAPS_FACTOR_ATTEMPTS*schedule.size()); 
                if(!improved)
                    delete test; 
            }
            
            if(improved)
                this->currentSolution = *test; 
        }
    
        /**
         * 
         * */
        void executeSolutionDestruction(){
            map<int, vector<Group> > schedule = currentSolution.getSchedule();
            vector<int> groupsByMachine = linearizeSchedule(schedule); 
            int numGroups = accumulate(groupsByMachine.begin(),groupsByMachine.end(),0);  
            double percentage[3] = {0.10, 0.15, 0.20};

            int cont=0; 
            int numRemotions = (int) 0.2*numGroups;; 

            for(int i=0;i<numRemotions; i++){
                int removedGroup = (int) percentage[rand()%3]*numGroups; 
                int sum =0;
                
                for(int j=0;j<groupsByMachine.size()-1; j++){
                    if(sum+groupsByMachine[j] < removedGroup){
                        sum+= groupsByMachine[j]; 
                    }else if(sum <= removedGroup && removedGroup < sum + groupsByMachine[j]  ){

                        NeighborhoodManager neighborhoodManager(input);
                        map<int,vector<Group> >::iterator map_it = schedule.begin();
                        advance(map_it,j);              // Get the machine element
                        int machine =  map_it->first;   // Get machine id
                        
                        this-> currentSolution  = *neighborhoodManager.deleteGroup(this->currentSolution, removedGroup - sum, machine);
                        groupsByMachine[j]--;
                        break;
                    }
                }    
            }
        }

        /**
         * 
         * */
        void executeSolutionReconstruction(){
            set<int> missingJobs = currentSolution.getMissingJobs(input->getNumberOfJobs()); 

            for(set<int>::iterator it = missingJobs.begin(); it != missingJobs.end(); it++){
                int job = *it; 
                vector<double> machineCosts = currentSolution.getMachineCosts(); 

                double minCost = 1e50; 
                int cheapestMachine  = 0; 

                for(int j=0; j<machineCosts.size(); j++){
                    if(machineCosts[j] < minCost ){
                        minCost = machineCosts[j];
                        cheapestMachine = j; 
                    }
                }
                
                insertIntoBestGroup(currentSolution, job, cheapestMachine); 
            }
        }




    public:
        MainHeuristic(){ }

        /**
         * 
         * */
        MainHeuristic(Input *input, Solution & bestSolution){
            this->input = input; 
            this->bestSolution = bestSolution; 
            this->currentSolution = bestSolution; 
            this->currentSolution.initializeMachineCosts(input->getNumberOfMachines());
            this->bestSolution.initializeMachineCosts(input->getNumberOfMachines());
        }

        /**
         * 
         * */
        MainHeuristic(Solution & bestSolution, Solution currentSolution){
            cerr<<"Warning!! Input not initialized\n";
            this->bestSolution = bestSolution; 
            this->currentSolution = currentSolution; 
        }

        Solution &getBestSolution(){ return bestSolution; }

        /**
         * 
         * */
        void execute(){
          
            if(bestSolution.isEmpty()){
                CreateInitialSolution(); 
                this->bestSolution = currentSolution;
            }

            if(currentSolution.isEmpty() ){
                this->currentSolution = this->bestSolution; 
            }
           
            int nj = input->getNumberOfJobs();
            int nm = input->getNumberOfMachines(); 
            
            int iterationsWithoutImprovement=0;
            double bestCostFound = __DBL_MAX__ ; 
           
            while(iterationsWithoutImprovement <= MAX_ITERATIONS_WITHOUT_IMPROVEMENT){
                #ifdef DEBUG
                    cout<<"Main while : " << iterationsWithoutImprovement <<" "<<bestSolution.getCost()<<endl; 
                #endif
                executeLocalSearch();
                executeSolutionDestruction();
                executeSolutionReconstruction();

                iterationsWithoutImprovement++;
                if(bestSolution.getCost() < bestCostFound){
                    iterationsWithoutImprovement = 0; 
                    bestCostFound = bestSolution.getCost(); 
                }
            }
        }
};



int main(int argc, char * argv[]){

    string listOfInstances;
    string instance; 
    
    //get the name of the file with the instance list
    if(argc > 1 ){
        string aux(argv[1]);
        listOfInstances = "./Instances/Instances With Deterioration/"+aux;	
    }else{
        cerr<<"Insert the instance list file name\n"; 
    }
    
    // open the instance list file
    ifstream arq(listOfInstances); 
    if(!arq.good())
        cerr<<"Instance list not found\n"; 

    //Read each instance and prints the result 
    ofstream res("results.txt");
    while(arq>>instance){

        Input input("./Instances/Instances With Deterioration/Instances/"+ instance);
        input.readInput(); 
        input.printInput(); 
        
        if(input.readSuccessfull()){
            Solution initialSolution; 
            MainHeuristic heuristic(&input, initialSolution); 
           
            auto start = high_resolution_clock::now(); 
            heuristic.execute(); 
            auto stop = high_resolution_clock::now();
            Solution bestSolution = heuristic.getBestSolution();
            
            auto duration = duration_cast<microseconds>(stop - start);
            cout<<duration.count()/1000000.0<<endl;
            res<<instance<< "  "<<bestSolution.getCost()<<" "<<duration.count()/1000000.0<<endl;
            cout<<instance<<endl;
            cout<<"Is feasible? " << bestSolution.isFeasible(input.getNumberOfJobs(), input.getNumberOfMachines())<<endl;
            bestSolution.printSolution(); 
            cout<<endl<<endl<<endl<<"_____________________________________________"<<endl; 
            //bestSolution.isFeasible(input.getNumberOfJobs(), input.getNumberOfMachines());
        }
    }

    return 0; 
}
