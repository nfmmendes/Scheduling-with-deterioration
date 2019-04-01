/**
	This instance generator works following the parameters of the article
	"Makespan mainimisation with sequence-dependent machine deterioration and maintenance events"
	by Alex J. Ruiz-Torres, Giuseppe Paletta, Rym M'Hallah  - 2017 , International Journal of Production Research
	
	but with different mainteinance times and deteriorations to each machine 
*/
#include<iostream>
#include<cstdlib>
#include<cmath>
#include<utility>
#include<algorithm>
#include<fstream>
using namespace std;




	int main(){
	
		const int MACHINE_CONFIGURATIONS = 4;
		const int JOBS_CONFIGURATIONS = 3;
		const int DETERIORATION_CONFIGURATIONS = 2;
		const int MAINTEINANCES_CONFIGURATIONS = 2;
		const int REPETITIONS = 10;
	
		const int numberOfMachines[MACHINE_CONFIGURATIONS] = {2, 5, 10, 20};
		const int jobsByMachineRatio[JOBS_CONFIGURATIONS] = {10, 15, 20};
		const pair<double,int> deteriorationIntervals[DETERIORATION_CONFIGURATIONS] = {make_pair(1.01, 6), make_pair(1.05, 10)};
		const int maintenances[MAINTEINANCES_CONFIGURATIONS] = {3, 9};
		
		

		for(int i=0;i<MACHINE_CONFIGURATIONS; i++){
			for(int j=0;j<JOBS_CONFIGURATIONS; j++){
				for(int k=0;k<DETERIORATION_CONFIGURATIONS;k++){
					for(int l = 0;l<MAINTEINANCES_CONFIGURATIONS; l++){
						for(int r=0;r<REPETITIONS;r++){
							string fileName = "m"  + to_string(numberOfMachines[i]) + "_j"+to_string(jobsByMachineRatio[j])+"_d"+to_string(k+1)+
											  "_mt"+ to_string(maintenances[l])+"_r"+to_string(r)+".txt";
							
							cout<<fileName<<endl;
							ofstream file;
							file.open(fileName);
							
							int numberOfJobs = jobsByMachineRatio[j]*numberOfMachines[i];
							
							file<<numberOfMachines[i]<<endl;
							file<<numberOfJobs<<endl;
							
							for(int n=0;n<numberOfJobs;n++)
								file<<(int) (1+rand()%100)<<" ";
							file<<endl; 
							
							for(int m = 0; m<numberOfMachines[i];m++)
								file<<(int) (1+rand()%maintenances[l])<<" ";
							file<<endl;
							
							for(int n=0;n<numberOfJobs;n++){
								for(int m=0;m<numberOfMachines[i];m++)
									file<<(double) deteriorationIntervals[k].first + (1.0*(rand()%deteriorationIntervals[k].second))/100<<" ";
								file<<endl;
							}
							
							file.close();
						}
					}
				}
			}
		}
	}