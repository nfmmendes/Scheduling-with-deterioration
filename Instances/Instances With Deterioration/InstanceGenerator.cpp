#include<iostream>
#include<fstream>
#include<string>
#include<cmath>
#include<cstdlib>
#include<vector>
using namespace std;


int main(){

	ifstream arq("instance_list.txt");
	int numMaquinas;
	int numJobs;
	vector<int> jobsDuration;
	vector<vector<int> > maintenanceTimes;
	string instanceName;
	
	if(arq.is_open()){
		while(arq>>instanceName){
			
			ifstream instance("Instances//"+instanceName);
			cout<<"Instances//"+instanceName<<" "<<instance.is_open()<<endl;
			if(instance.is_open()){
				
				//Write the original instance data
				instance>>numMaquinas>>numJobs;
				jobsDuration.clear();
				jobsDuration.resize(numJobs);
				for(int i=0;i<numJobs;i++)
					instance>>jobsDuration[i];
			
				int biggerJob=-1;
				int smallerJob=100000000;
				
				for(int i=0;i<jobsDuration.size();i++){
					if(biggerJob < jobsDuration[i])
						biggerJob=jobsDuration[i];
					if(smallerJob > jobsDuration[i])
						smallerJob = jobsDuration[i];
				}
				
				maintenanceTimes.resize(5);
				int range = (biggerJob - smallerJob)/2;
				
				if(range <= 1)
					range = biggerJob/10+1;
				
				for(int i=0;i<5;i++){
					maintenanceTimes[i].resize(numMaquinas);
					for(int j=0;j<numMaquinas;j++)
						maintenanceTimes[i][j] = rand()%range+1;
				}
					
			
			
				instance.close();
			
			
				//Write the instance data
				for(int k = 0;k<5;k++){
					ofstream newInstance("Instances//"+to_string( (char)('A'+k))+"_"+instanceName );
				
					if(newInstance.is_open()){
						
						newInstance<<numMaquinas<<endl<<numJobs<<endl;
						for(int i=0;i<numJobs;i++)
							newInstance<<jobsDuration[i]<<" ";
						newInstance<<endl;
						
						for(int i=0;i<numMaquinas;i++)
							newInstance<<maintenanceTimes[k][i]<<" ";
						newInstance<<endl;
						
						for(int i=0;i<numJobs;i++){
							for(int j=0;j<numMaquinas;j++){
								newInstance<<1.0+(rand()%500)*1.0/10000<<" ";
							}
							newInstance<<endl;
						}
						
						newInstance.close();
					}
				}
				
			}
		}
	}
		
	
}