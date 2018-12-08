
libname mylib 'M:/506/hw/hw4/STATS506/HW4';
DATA mylib.Medicare_PS_PUF;
	LENGTH
		npi              					$ 10
		nppes_provider_last_org_name 		$ 70
		nppes_provider_first_name 			$ 20
		nppes_provider_mi					$ 1
		nppes_credentials 					$ 20
		nppes_provider_gender				$ 1
		nppes_entity_code 					$ 1
		nppes_provider_street1 				$ 55
		nppes_provider_street2				$ 55
		nppes_provider_city 				$ 40
		nppes_provider_zip 					$ 20
		nppes_provider_state				$ 2
		nppes_provider_country				$ 2
		provider_type 						$ 55
		medicare_participation_indicator 	$ 1
		place_of_service					$ 1
		hcpcs_code       					$ 5
		hcpcs_description 					$ 256
		hcpcs_drug_indicator				$ 1
		line_srvc_cnt      					8
		bene_unique_cnt    					8
		bene_day_srvc_cnt   				8
		average_Medicare_allowed_amt   		8
		average_submitted_chrg_amt  		8
		average_Medicare_payment_amt   		8
		average_Medicare_standard_amt		8;
	INFILE 'M:\506\hw\hw4\STATS506\HW4\problem3\data\Medicare_Provider_Util_Payment_PUF_CY2016.txt'

		lrecl=32767
		dlm='09'x
		pad missover
		firstobs = 3
		dsd;

	INPUT
		npi             
		nppes_provider_last_org_name 
		nppes_provider_first_name 
		nppes_provider_mi 
		nppes_credentials 
		nppes_provider_gender 
		nppes_entity_code 
		nppes_provider_street1 
		nppes_provider_street2 
		nppes_provider_city 
		nppes_provider_zip 
		nppes_provider_state 
		nppes_provider_country 
		provider_type 
		medicare_participation_indicator 
		place_of_service 
		hcpcs_code       
		hcpcs_description 
		hcpcs_drug_indicator
		line_srvc_cnt    
		bene_unique_cnt  
		bene_day_srvc_cnt 
		average_Medicare_allowed_amt 
		average_submitted_chrg_amt 
		average_Medicare_payment_amt
		average_Medicare_standard_amt;

	LABEL
		npi     							= "National Provider Identifier"       
		nppes_provider_last_org_name 		= "Last Name/Organization Name of the Provider"
		nppes_provider_first_name 			= "First Name of the Provider"
		nppes_provider_mi					= "Middle Initial of the Provider"
		nppes_credentials 					= "Credentials of the Provider"
		nppes_provider_gender 				= "Gender of the Provider"
		nppes_entity_code 					= "Entity Type of the Provider"
		nppes_provider_street1 				= "Street Address 1 of the Provider"
		nppes_provider_street2 				= "Street Address 2 of the Provider"
		nppes_provider_city 				= "City of the Provider"
		nppes_provider_zip 					= "Zip Code of the Provider"
		nppes_provider_state 				= "State Code of the Provider"
		nppes_provider_country 				= "Country Code of the Provider"
		provider_type	 					= "Provider Type of the Provider"
		medicare_participation_indicator 	= "Medicare Participation Indicator"
		place_of_service 					= "Place of Service"
		hcpcs_code       					= "HCPCS Code"
		hcpcs_description 					= "HCPCS Description"
		hcpcs_drug_indicator				= "Identifies HCPCS As Drug Included in the ASP Drug List"
		line_srvc_cnt    					= "Number of Services"
		bene_unique_cnt  					= "Number of Medicare Beneficiaries"
		bene_day_srvc_cnt 					= "Number of Distinct Medicare Beneficiary/Per Day Services"
		average_Medicare_allowed_amt 		= "Average Medicare Allowed Amount"
		average_submitted_chrg_amt 			= "Average Submitted Charge Amount"
		average_Medicare_payment_amt 		= "Average Medicare Payment Amount"
		average_Medicare_standard_amt		= "Average Medicare Standardized Payment Amount";
RUN;

/*the following code is for q3(c)*/

/*The first step is to filter the data based on the criterion*/
data medicare;
  set mylib.medicare_ps_puf;
  where hcpcs_description contains 'MRI' and hcpcs_code like '7%';

/*Then, we calculate the total number of services per MRI operation,
  the total payment of each operation and the average payment per service*/

proc summary data=medicare sum;
class hcpcs_description;
output out=result
sum(line_srvc_cnt) = volume
sum(average_Medicare_payment_amt*line_srvc_cnt) = total_payment
mean(average_Medicare_payment_amt*line_srvc_cnt)/volume = average_payment;

proc print data=result;
run;
data ps4_q3c;
set result;
if _TYPE_=1;

/*To take a brief look at the result, we sort the summary table by volume in descending order,
and we could do this as well for the other two variables, which we leave out at the current stage*/

proc sort data=ps4_q3c out=ps4_q3c_result;
by descending volume;
/* Finally, we write the result into a designated csv file.*/

proc export data=ps4_q3c_result
  outfile='M:/506/hw/hw4/STATS506/HW4/problem3/ps4_q3c.csv'
  dbms=csv
  replace;

  run;


/*Now, we are going to replicate what we have done in the b and c by using PROC SQL instead:*/
proc sql;
  create table ps4_q3d as
      select hcpcs_description,sum(line_srvc_cnt) as volume, sum(average_Medicare_payment_amt*line_srvc_cnt) as total_payment
      from medicare
      where hcpcs_code like "7%" and hcpcs_description like "%MRI%"
      group by hcpcs_description
      order by -volume;

quit;

proc print data=ps4_q3d(obs=5);

run;



