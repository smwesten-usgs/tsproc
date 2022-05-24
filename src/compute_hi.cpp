//		Compute Hydrologic Indices

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <iostream>
using namespace std;

//extern bool usemedian;		// Flag indicating median is to be used in computing MA3,MA12-MA35,ML1-ML12,ML14,
							//  ML14,ML15,ML17,ML19,ML22,MH1-MH12,MH20,FL1,FL3,FH1,FH3-FH11,DL1-DL5,DL18,
							//  DH17-DH24,RA1,RA3,RA8

void stats (int, double*, double*, double*, double*);
double percentile(double, int, double*);
void order(int,double*,int);

//void compute_hi(int datatype, double carea,
//		double m_lp, double m_up, int yr[], double q[][366])

extern "C" void compute_hi(bool usemedian, double carea, double nearhuge,
		double m_lp, double m_up, int yr[150], double q[150][366],
		double MA[46], double LMA[46], double UMA[46],
		double ML[23],double LML[23],double UML[23],
		double MH[28],double LMH[28],double UMH[28],
		double FL[4],double LFL[4],double UFL[4],
		double FH[12],double LFH[12],double UFH[12],
		double DL[21],double LDL[21],double UDL[21],
		double DH[25],double LDH[25],double UDH[25],
		double TA[4],double LTA[4],double UTA[4],
		double TL[5],double LTL[5],double UTL[5],
		double TH[4],double LTH[4],double UTH[4],
		double RA[10], double LRA[10], double URA[10])
{

	int day;			// Day of the year index
	int nyrs;			// Number of years
	int i, j, k;
	bool dopeak;		// Flag indicating that indices using peak flow are to be computed
	double lb, ub;		// Lower and upper limit percentile fractions
	double peak[150];	// Annual peak hourly flows
	double f24[150];		// Average daily flow for the peak flow day
	int npyrs;			// Number of years of peak flow data

	lb = m_lp / 100;
	ub = m_up / 100;

    for(i=0;i<46;i++) {LMA[i]=nearhuge;UMA[i]=nearhuge;}
    for(i=0;i<23;i++) {LML[i]=nearhuge;UML[i]=nearhuge;}
    for(i=0;i<28;i++) {LMH[i]=nearhuge;UMH[i]=nearhuge;}
    for(i=0;i<4;i++) {LFL[i]=nearhuge;UFL[i]=nearhuge;}
    for(i=0;i<12;i++) {LFH[i]=nearhuge;UFH[i]=nearhuge;}
    for(i=0;i<21;i++) {LDL[i]=nearhuge;UDL[i]=nearhuge;}
    for(i=0;i<25;i++) {LDH[i]=nearhuge;UDH[i]=nearhuge;}
    for(i=0;i<4;i++) {LTA[i]=nearhuge;UTA[i]=nearhuge;}
    for(i=0;i<5;i++) {LTL[i]=nearhuge;UTL[i]=nearhuge;}
    for(i=0;i<4;i++) {LTH[i]=nearhuge;UTH[i]=nearhuge;}
    for(i=0;i<10;i++) {LRA[i]=nearhuge;URA[i]=nearhuge;}

// Read the flow data

// Read the years

// Find the number of years

	nyrs = 0;
	for(i=0;i<150;i++)
	{
		if(yr[i]==0) break;
		nyrs++;
	}

// Read the flow data


// Read the peak flow data if there is any

	dopeak = false;
//	if(datatype == 1)

// Set any negative flows to missing data

	for(i=0;i<nyrs;i++)
	{
		for(j=0;j<366;j++)

		{
			if(q[i][j] < 0.) q[i][j] = nearhuge + 1;
		}
	}

// Open the output file


// Begin calculation of the indices (average flow conditions)

/*	double MA1, LMA1, UMA1;		// Average flow for all flows
 *	double MA2, LMA2, UMA2;		// Median flow for all flows
 *	double MA3, LMA3, UMA3;		// Variability in daily flows 1 (coefficient of variation)
 *	double MA4;					// Variability in daily flows 2
 *	double MA5;					// Skewness in daily flows
 *	double MA6, MA7, MA8;		// Ranges in daily flows
 *	double MA9, MA10, MA11;		// Spreads in daily flows
 *	double MA12, LMA12, UMA12;	// January means
 *	double MA13, LMA13, UMA13;	// February means
 *	double MA14, LMA14, UMA14;	// March means
 *	double MA15, LMA15, UMA15;	// April means
 *	double MA16, LMA16, UMA16;	// May means
 *	double MA17, LMA17, UMA17;	// June means
 *	double MA18, LMA18, UMA18;	// July means
 *	double MA19, LMA19, UMA19;	// August means
 *	double MA20, LMA20, UMA20;	// September means
 *	double MA21, LMA21, UMA21;	// October means
 *	double MA22, LMA22, UMA22;	// November means
 *	double MA23, LMA23, UMA23;	// December means
 *	double MA24, LMA24, UMA24;	// January coefficient of variation
 *	double MA25, LMA25, UMA25;	// February coefficient of variation
 *	double MA26, LMA26, UMA26;	// March coefficient of variation
 *	double MA27, LMA27, UMA27;	// April coefficient of variation
 *	double MA28, LMA28, UMA28;	// May coefficient of variation
 *	double MA29, LMA29, UMA29;	// June coefficient of variation
 *	double MA30, LMA30, UMA30;	// July coefficient of variation
 *	double MA31, LMA31, UMA31;	// August coefficient of variation
 *	double MA32, LMA32, UMA32;	// September coefficient of variation
 *	double MA33, LMA33, UMA33;	// October coefficient of variation
 *	double MA34, LMA34, UMA34;	// November coefficient of variation
 *	double MA35, LMA35, UMA35;	// December coefficient of variation
 *	double MA36;		// Variability across monthly flows 1 (range)
 *	double MA37;		// Variability across monthly flows 1 (interquartile)
 *	double MA38;		// Variability across monthly flows 1 (90th - 10th percentile)
 *	double MA39;		// Variability across monthly flows 2 (CV in monthly mean flows)
 *	double MA40;		// Skewness in monthly flows
 *	double MA41, LMA41, UMA41;		// Mean annual runoff
 *	double MA42;		// Variability across annual flows 1 (range)
 *	double MA43;		// Variability across annual flows 1 (interquartile)
 *	double MA44;		// Variability across annual flows 1 (90th - 10th percentile)
 *	double MA45;		// Skewness in annual flows */

	int nval;		// Max number of values

	nval = nyrs * 366;
	//data = new double[nval];
    double *data = (double *) malloc(nval * sizeof (double));
    if (data == NULL)
    {
        printf("Could not allocate memory");
        exit(EXIT_FAILURE);
    }
	//mdata = new double[nval];
    double *mdata = (double *) malloc(nval * sizeof (double));
    if (mdata == NULL)
    {
        printf("Could not allocate memory");
        exit(EXIT_FAILURE);
    }
	//baseflow = new double[nval];
    double *baseflow = (double *) malloc(nval * sizeof (double));
    if (baseflow == NULL)
    {
        printf("Could not allocate memory");
        exit(EXIT_FAILURE);
    }

// Remove missing data

	int ndv = 0;		// Number of data values
	for(i=0;i<nyrs;i++)
	{
		for(j=0;j<366;j++)
		{
//			printf("YEAR: %i  DAY: %i  VALUE: %12.3f\n", i,j,q[i][j]);
			if(q[i][j]  < nearhuge)
			{
				data[ndv] = q[i][j];
				ndv++;
			}
		}
	}

// Compute the basic statistics (MA[1] and MA[2])
// MA[1]		Mean for the entire flow record.  (cfs)

// MA[2]		Median for the entire flow record.  (cfs)


//	stats(ndv,data,&MA[1],&MA[2],&MA3);
	stats(ndv,data,&MA[1],&MA[2],&MA[3]);
	LMA[1] = LMA[2] = percentile(lb,ndv,data);
	if(LMA[1] > MA[1]) LMA[1] = MA[1];
	if(UMA[1] < MA[1]) UMA[1] = MA[1];
	UMA[1] = UMA[2] = percentile(ub,ndv,data);
	if(LMA[2] > MA[2]) LMA[2] = MA[2];
	if(UMA[2] < MA[2]) UMA[2] = MA[2];

// Compute daily variability 1 (coefficient of variation = standard deviation/mean * 100)

// MA3		Mean of the coefficients of variation (standard deviation/mean) for each year.  Compute the
//			coefficient of variation for each year of flows.  Compute the mean of the annual coefficients
//			of variation.  (%)

	double ydata[366];		// Daily flow values for a year
	double stdv[150];		// Standard deviation for each year
	double qmean;			// Mean of the monthly means
	double qmedian;			// Median of the monthly means
	double qstdv;			// Standard deviation of the monthly means
	double temp;

	for(i=0;i<nyrs;i++)
	{
		k = 0;
		for(j=0;j<366;j++)
		{
			if(q[i][j]  < nearhuge)
			{
				ydata[k] = q[i][j];
				k++;
			}
		}
		stats(k,ydata,&qmean,&qmedian,&stdv[i]);
	}
	stats(nyrs,stdv,&qmean,&qmedian,&qstdv);
	MA[3] = 999999;
	if(MA[1] != 0.) MA[3] = (qmean/MA[1])*100.;
	temp = percentile(lb,nyrs,stdv);
	LMA[3] = 999999;
	if(MA[1] != 0.) LMA[3] = 100*temp/MA[1];
	temp = percentile(ub,nyrs,stdv);
	UMA[3] = 999999;
	if(MA[1] != 0.) UMA[3] = 100*temp/MA[1];
	if(LMA[3] > MA[3]) LMA[3] = MA[3];
	if(UMA[3] < MA[3]) UMA[3] = MA[3];

// Compute daily variability 2 (coefficient of variation = std dev. of 19 percentiles / mean of 19 percentiles

// MA4  	Standard deviation of the percentiles of the entire flow record divided by the mean of
//			percentiles.  Compute the 5th, 10th, 15th, 20th, 25th, 30th, 35th, 40th, 45th, 50th,
//			55th, 60th, 65th, 70th, 75th, 80th, 85th, 90th, and 95th percentiles for the entire flow record.
//			Percentiles are computed by interpolating between the ordered (ascending) logs of the flow values.
//			Compute the standard deviation and mean for the percentile values.
//			Divide the standard deviation by the mean to get MA4.  	(%)
//			Limits are not computed.

// Compute percentiles (5th, 10th,...,95th)

	double p[19] = {.05,.1,.15,.2,.25,.3,.35,.4,.45,.5,.55,.6,.65,.7,.75,.8,.85,.9,.95};
	double pctile[19];		// Array of calculated percentiles
	double meanp;			// Mean of the calculated percentiles
	double medianp;			// Median of the calculated percentiles
	double stdp;				// Standard deviation of the calculated percentiles

	order(1,data,ndv);		// Order data in ascending order
	for(i=0;i<19;i++)
	{
		pctile[i] = percentile(p[i],ndv,data);
	}

	stats(19,pctile,&meanp,&medianp,&stdp);
	MA[4] = 999999;
	if(meanp != 0.) MA[4] = (stdp/meanp)*100.;

// Compute the skewness in daily flows (mean / median)

// MA5		The skewness of the entire flow record is computed as the mean for the entire record (MA[1])
//			Divided by the median (MA[2]).  (dimensionless)
//			Limits are not computed.

	MA[5] = 999999;
	if(MA[2] != 0.) MA[5] = MA[1] / MA[2];

// Compute exceedence for the entire record

	order(0,data,ndv);		// Order data in descending order
	for(i=0;i<19;i++)
	{
		pctile[i] = percentile(p[i],ndv,data);
	}

// Compute the ranges in daily flows

// MA6		Range in daily flows 1 is the ratio of the 10% to 90% exceedence values for the entire flow
//			record.  Compute the 5% to 95% exceedence values for the entire flow record.  Exceedence
//			is computed by interpolating between the ordered (descending) flow values.  Divide the 10%
//			exceedence value by the 90% value.  (dimensionless)
//			Limits are not computed.

	MA[6] = 999999;
	if(pctile[17] != 0.) MA[6] = pctile[1]/pctile[17];		// 10th / 90th

// MA7		Range in daily flows 2 is computed like MA6 except using the 20% and 80% exceedence values.
//			Divide the 20% exceedence value by the 80% value.  (dimensionless)
//			Limits are not computed.

	MA[7] = 999999;
	if(pctile[15] != 0.)MA[7] = pctile[3]/pctile[15];		// 20th / 80th

// MA8		Range in daily flows 3 is computed like MA6 except using the 25% and 75% exceedence values.
//			Divide the 25% exceedence value by the 75% value.  (dimensionless)

	MA[8] = 999999;
	if(pctile[14] != 0.)MA[8] = pctile[4]/pctile[14];		// 25th / 75th

// Compute the spreads in daily flows (range / median)

	order(1,data,ndv);		// Order data in ascending order
	for(i=0;i<19;i++)
	{
		pctile[i] = percentile(p[i],ndv,data);
	}

// MA9		Spread in daily flows 1 is the ratio of the difference between the 90th and 10th percentile
//			of the flow data to the median of the entire flow record.  Compute the 5th, 10th, 15th, 20th,
//			25th, 30th, 35th, 40th, 45th, 50th, 55th, 60th, 65th, 70th, 75th, 80th, 85th, 90th, and 95th
//			percentiles for the entire flow record.  Percentiles are computed by interpolating
//			between the ordered (ascending) flow values.
//			Compute MA9 as (90th - 10th) / MA[2].  (dimensionless)
//			Limits are not computed.

	MA[9] = 999999;
	if(MA[2]!=0) MA[9] = (pctile[17]-pctile[1]) / MA[2];

// MA10		Spread in daily flows 2 is computed like MA9 except using the 20th and 80th percentiles.
//			(dimensionless) Limits are not computed.

	MA[10] = 999999;
	if(MA[2]!=0) MA[10] = (pctile[15]-pctile[3]) / MA[2];

// MA11		Spread in daily flows 3 is computed like MA9 except using the 25th and 75th percentiles.
//			(dimensionless) Limits are not computed.

	MA[11] = 999999;
	if(MA[2]!=0) MA[11] = (pctile[14]-pctile[4]) / MA[2];

// Calculate monthly means and monthly coefficients of variation

// MA12 -	Means of monthly flow values.  Compute the means for each month over the entire flow record.
//  MA23	For example, MA12 is the mean of all January flow values over the entire record.  (cfs)

// MA24 -	Variability (coefficient of variation) of monthly flow values.  Compute the standard deviation for
//   MA35	each month in each year over the entire flow record.  Divide the standard deviation by the mean 			for each month.  Average these values for each month across all years.  (%)

	int nmv;		// Number of monthly values
	int zcount;		// Number of months with flow = 0;
	double total;
	double total2;
	double dum;				// Dummy variable

// Compile a data set for each month and compute the means and CV

// January
	zcount = 0;		// Number of zero flow months
	for(i=0;i<nyrs;i++)
	{
		total = 0.;
		nmv = 0;
		for(j=92;j<123;j++)
		{
			if(q[i][j]  < nearhuge)
			{
				mdata[nmv] = q[i][j];
				total = total + mdata[nmv];
				nmv++;
			}
		}
		stats(nmv,mdata,&qmean,&dum,&qstdv);
		if(qmean != 0.)
			stdv[i] = 100*qstdv/qmean;
		else
			stdv[i] = 0.;
		if(nmv > 0)
			ydata[i] = total/nmv;
		else
			ydata[i] = 0.;
		if(total==0.) zcount++;
	}
	stats(nyrs,ydata,&qmean,&qmedian,&qstdv);
	MA[12] = qmean;
	if(usemedian) MA[12] = qmedian;
	LMA[12] = percentile(lb,nyrs,ydata);
	UMA[12] = percentile(ub,nyrs,ydata);
	if(LMA[12] > MA[12]) LMA[12] = MA[12];
	if(UMA[12] < MA[12]) UMA[12] = MA[12];


	stats(nyrs,stdv,&qmean,&dum,&qstdv);
	MA[24] = qmean;		// Coefficient of variation
	if(usemedian) MA[24] = dum;
	LMA[24] = percentile(lb,nyrs,stdv);
	UMA[24] = percentile(ub,nyrs,stdv);
	if(LMA[24] > MA[24]) LMA[24] = MA[24];
	if(UMA[24] < MA[24]) UMA[24] = MA[24];

// February
	for(i=0;i<nyrs;i++)
	{
		total = 0.;
		nmv = 0;
		for(j=123;j<152;j++)
		{
//  			printf("YEAR: %i  DAY: %i  VALUE: %12.3f\n", i,j,q[i][j]);
			if(q[i][j]  < nearhuge)
			{
				mdata[nmv] = q[i][j];
				total = total + mdata[nmv];
				nmv++;
			}
		}
		stats(nmv,mdata,&qmean,&dum,&qstdv);
		if(qmean != 0.)
			stdv[i] = 100*qstdv/qmean;
		else
			stdv[i] = 0.;
		if(nmv > 0)
			ydata[i] = total/nmv;
		else
			ydata[i] = 0.;
		if(total==0.) zcount++;
	}
	stats(nyrs,ydata,&qmean,&qmedian,&qstdv);
	MA[13] = qmean;
	if(usemedian) MA[13] = qmedian;
	LMA[13] = percentile(lb,nyrs,ydata);
	UMA[13] = percentile(ub,nyrs,ydata);
	if(LMA[13] > MA[13]) LMA[13] = MA[13];
	if(UMA[13] < MA[13]) UMA[13] = MA[13];

	stats(nyrs,stdv,&qmean,&dum,&qstdv);
	MA[25] = qmean;		// Coefficient of variation
	if(usemedian) MA[25] = dum;
	LMA[25] = percentile(lb,nyrs,stdv);
	UMA[25] = percentile(ub,nyrs,stdv);
	if(LMA[25] > MA[25]) LMA[25] = MA[25];
	if(UMA[25] < MA[25]) UMA[25] = MA[25];

// March
	for(i=0;i<nyrs;i++)
	{
		total = 0.;
		nmv = 0;
		for(j=152;j<183;j++)
		{
			if(q[i][j]  < nearhuge)
			{
				mdata[nmv] = q[i][j];
				total = total + mdata[nmv];
				nmv++;
			}
		}
		stats(nmv,mdata,&qmean,&dum,&qstdv);
		if(qmean != 0.)
			stdv[i] = 100*qstdv/qmean;
		else
			stdv[i] = 0.;
		if(nmv > 0)
			ydata[i] = total/nmv;
		else
			ydata[i] = 0.;
		if(total==0.) zcount++;
	}
	stats(nyrs,ydata,&qmean,&qmedian,&qstdv);
	MA[14] = qmean;
	if(usemedian) MA[14] = qmedian;
	LMA[14] = percentile(lb,nyrs,ydata);
	UMA[14] = percentile(ub,nyrs,ydata);
	if(LMA[14] > MA[14]) LMA[14] = MA[14];
	if(UMA[14] < MA[14]) UMA[14] = MA[14];

	stats(nyrs,stdv,&qmean,&dum,&qstdv);
	MA[26] = qmean;		// Coefficient of variation
	if(usemedian) MA[26] = dum;
	LMA[26] = percentile(lb,nyrs,stdv);
	UMA[26] = percentile(ub,nyrs,stdv);
	if(LMA[26] > MA[26]) LMA[26] = MA[26];
	if(UMA[26] < MA[26]) UMA[26] = MA[26];

// April
	for(i=0;i<nyrs;i++)
	{
		total = 0.;
		nmv = 0;
		for(j=183;j<213;j++)
		{
			if(q[i][j]  < nearhuge)
			{
				mdata[nmv] = q[i][j];
				total = total + mdata[nmv];
				nmv++;
			}
		}
		stats(nmv,mdata,&qmean,&dum,&qstdv);
		if(qmean != 0.)
			stdv[i] = 100*qstdv/qmean;
		else
			stdv[i] = 0.;
		if(nmv > 0)
			ydata[i] = total/nmv;
		else
			ydata[i] = 0.;
		if(total==0.) zcount++;
	}
	stats(nyrs,ydata,&qmean,&qmedian,&qstdv);
	MA[15] = qmean;
	if(usemedian) MA[15] = qmedian;
	LMA[15] = percentile(lb,nyrs,ydata);
	UMA[15] = percentile(ub,nyrs,ydata);
	if(LMA[15] > MA[15]) LMA[15] = MA[15];
	if(UMA[15] < MA[15]) UMA[15] = MA[15];

	stats(nyrs,stdv,&qmean,&dum,&qstdv);
	MA[27] = qmean;		// Coefficient of variation
	if(usemedian) MA[27] = dum;
	LMA[27] = percentile(lb,nyrs,stdv);
	UMA[27] = percentile(ub,nyrs,stdv);
	if(LMA[27] > MA[27]) LMA[27] = MA[27];
	if(UMA[27] < MA[27]) UMA[27] = MA[27];

// May
	for(i=0;i<nyrs;i++)
	{
		total = 0.;
		nmv = 0;
		for(j=213;j<244;j++)
		{
			if(q[i][j]  < nearhuge)
			{
				mdata[nmv] = q[i][j];
				total = total + mdata[nmv];
				nmv++;
			}
		}
		stats(nmv,mdata,&qmean,&dum,&qstdv);
		if(qmean != 0.)
			stdv[i] = 100*qstdv/qmean;
		else
			stdv[i] = 0.;
		if(nmv > 0)
			ydata[i] = total/nmv;
		else
			ydata[i] = 0.;
		if(total==0.) zcount++;
	}
	stats(nyrs,ydata,&qmean,&qmedian,&qstdv);
	MA[16] = qmean;
	if(usemedian) MA[16] = qmedian;
	LMA[16] = percentile(lb,nyrs,ydata);
	UMA[16] = percentile(ub,nyrs,ydata);
	if(LMA[16] > MA[16]) LMA[16] = MA[16];
	if(UMA[16] < MA[16]) UMA[16] = MA[16];

	stats(nyrs,stdv,&qmean,&dum,&qstdv);
	MA[28] = qmean;		// Coefficient of variation
	if(usemedian) MA[28] = dum;
	LMA[28] = percentile(lb,nyrs,stdv);
	UMA[28] = percentile(ub,nyrs,stdv);
	if(LMA[28] > MA[28]) LMA[28] = MA[28];
	if(UMA[28] < MA[28]) UMA[28] = MA[28];

// June
	for(i=0;i<nyrs;i++)
	{
		total = 0.;
		nmv = 0;
		for(j=244;j<274;j++)
		{
			if(q[i][j]  < nearhuge)
			{
				mdata[nmv] = q[i][j];
				total = total + mdata[nmv];
				nmv++;
			}
		}
		stats(nmv,mdata,&qmean,&dum,&qstdv);
		if(qmean != 0.)
			stdv[i] = 100*qstdv/qmean;
		else
			stdv[i] = 0.;
		if(nmv > 0)
			ydata[i] = total/nmv;
		else
			ydata[i] = 0.;
		if(total==0.) zcount++;
	}
	stats(nyrs,ydata,&qmean,&qmedian,&qstdv);
	MA[17] = qmean;
	if(usemedian) MA[17] = qmedian;
	LMA[17] = percentile(lb,nyrs,ydata);
	UMA[17] = percentile(ub,nyrs,ydata);
	if(LMA[17] > MA[17]) LMA[17] = MA[17];
	if(UMA[17] < MA[17]) UMA[17] = MA[17];

	stats(nyrs,stdv,&qmean,&dum,&qstdv);
	MA[29] = qmean;		// Coefficient of variation
	if(usemedian) MA[29] = dum;
	LMA[29] = percentile(lb,nyrs,stdv);
	UMA[29] = percentile(ub,nyrs,stdv);
	if(LMA[29] > MA[29]) LMA[29] = MA[29];
	if(UMA[29] < MA[29]) UMA[29] = MA[29];

// July
	for(i=0;i<nyrs;i++)
	{
		total = 0.;
		nmv = 0;
		for(j=274;j<305;j++)
		{
			if(q[i][j]  < nearhuge)
			{
				mdata[nmv] = q[i][j];
				total = total + mdata[nmv];
				nmv++;
			}
		}
		stats(nmv,mdata,&qmean,&dum,&qstdv);
		if(qmean != 0.)
			stdv[i] = 100*qstdv/qmean;
		else
			stdv[i] = 0.;
		if(nmv > 0)
			ydata[i] = total/nmv;
		else
			ydata[i] = 0.;
		if(total==0.) zcount++;
	}
	stats(nyrs,ydata,&qmean,&qmedian,&qstdv);
	MA[18] = qmean;
	if(usemedian) MA[18] = qmedian;
	LMA[18] = percentile(lb,nyrs,ydata);
	UMA[18] = percentile(ub,nyrs,ydata);
	if(LMA[18] > MA[18]) LMA[18] = MA[18];
	if(UMA[18] < MA[18]) UMA[18] = MA[18];

	stats(nyrs,stdv,&qmean,&dum,&qstdv);
	MA[30] = qmean;		// Coefficient of variation
	if(usemedian) MA[30] = dum;
	LMA[30] = percentile(lb,nyrs,stdv);
	UMA[30] = percentile(ub,nyrs,stdv);
	if(LMA[30] > MA[30]) LMA[30] = MA[30];
	if(UMA[30] < MA[30]) UMA[30] = MA[30];

// August
	for(i=0;i<nyrs;i++)
	{
		total = 0.;
		nmv = 0;
		for(j=305;j<336;j++)
		{
			if(q[i][j]  < nearhuge)
			{
				mdata[nmv] = q[i][j];
				total = total + mdata[nmv];
				nmv++;
			}
		}
		stats(nmv,mdata,&qmean,&dum,&qstdv);
		if(qmean != 0.)
			stdv[i] = 100*qstdv/qmean;
		else
			stdv[i] = 0.;
		if(nmv > 0)
			ydata[i] = total/nmv;
		else
			ydata[i] = 0.;
		if(total==0.) zcount++;
	}
	stats(nyrs,ydata,&qmean,&qmedian,&qstdv);
	MA[19] = qmean;
	if(usemedian) MA[19] = qmedian;
	LMA[19] = percentile(lb,nyrs,ydata);
	UMA[19] = percentile(ub,nyrs,ydata);
	if(LMA[19] > MA[19]) LMA[19] = MA[19];
	if(UMA[19] < MA[19]) UMA[19] = MA[19];

	stats(nyrs,stdv,&qmean,&dum,&qstdv);
	MA[31] = qmean;		// Coefficient of variation
	if(usemedian) MA[31] = dum;
	LMA[31] = percentile(lb,nyrs,stdv);
	UMA[31] = percentile(ub,nyrs,stdv);
	if(LMA[31] > MA[31]) LMA[31] = MA[31];
	if(UMA[31] < MA[31]) UMA[31] = MA[31];

// September
	for(i=0;i<nyrs;i++)
	{
		total = 0.;
		nmv = 0;
		for(j=336;j<366;j++)
		{
			if(q[i][j]  < nearhuge)
			{
				mdata[nmv] = q[i][j];
				total = total + mdata[nmv];
				nmv++;
			}
		}
		stats(nmv,mdata,&qmean,&dum,&qstdv);
		if(qmean != 0.)
			stdv[i] = 100*qstdv/qmean;
		else
			stdv[i] = 0.;
		if(nmv > 0)
			ydata[i] = total/nmv;
		else
			ydata[i] = 0.;
		if(total==0.) zcount++;
	}
	stats(nyrs,ydata,&qmean,&qmedian,&qstdv);
	MA[20] = qmean;
	if(usemedian) MA[20] = qmedian;
	LMA[20] = percentile(lb,nyrs,ydata);
	UMA[20] = percentile(ub,nyrs,ydata);
	if(LMA[20] > MA[20]) LMA[20] = MA[20];
	if(UMA[20] < MA[20]) UMA[20] = MA[20];

	stats(nyrs,stdv,&qmean,&dum,&qstdv);
	MA[32] = qmean;		// Coefficient of variation
	if(usemedian) MA[32] = dum;
	LMA[32] = percentile(lb,nyrs,stdv);
	UMA[32] = percentile(ub,nyrs,stdv);
	if(LMA[32] > MA[32]) LMA[32] = MA[32];
	if(UMA[32] < MA[32]) UMA[32] = MA[32];

// October
	for(i=0;i<nyrs;i++)
	{
		total = 0.;
		nmv = 0;
		for(j=0;j<31;j++)
		{
			if(q[i][j]  < nearhuge)
			{
				mdata[nmv] = q[i][j];
				total = total + mdata[nmv];
				nmv++;
			}
		}
		stats(nmv,mdata,&qmean,&dum,&qstdv);
		if(qmean != 0.)
			stdv[i] = 100*qstdv/qmean;
		else
			stdv[i] = 0.;
		if(nmv > 0)
			ydata[i] = total/nmv;
		else
			ydata[i] = 0.;
		if(total==0.) zcount++;
	}
	stats(nyrs,ydata,&qmean,&qmedian,&qstdv);
	MA[21] = qmean;
	if(usemedian) MA[21] = qmedian;
	LMA[21] = percentile(lb,nyrs,ydata);
	UMA[21] = percentile(ub,nyrs,ydata);
	if(LMA[21] > MA[21]) LMA[21] = MA[21];
	if(UMA[21] < MA[21]) UMA[21] = MA[21];

	stats(nyrs,stdv,&qmean,&dum,&qstdv);
	MA[33] = qmean;		// Coefficient of variation
	if(usemedian) MA[33] = dum;
	LMA[33] = percentile(lb,nyrs,stdv);
	UMA[33] = percentile(ub,nyrs,stdv);
	if(LMA[33] > MA[33]) LMA[33] = MA[33];
	if(UMA[33] < MA[33]) UMA[33] = MA[33];

// November
	for(i=0;i<nyrs;i++)
	{
		total = 0.;
		nmv = 0;
		for(j=31;j<61;j++)
		{
			if(q[i][j]  < nearhuge)
			{
				mdata[nmv] = q[i][j];
				total = total + mdata[nmv];
				nmv++;
			}
		}
		stats(nmv,mdata,&qmean,&dum,&qstdv);
		if(qmean != 0.)
			stdv[i] = 100*qstdv/qmean;
		else
			stdv[i] = 0.;
		if(nmv > 0)
			ydata[i] = total/nmv;
		else
			ydata[i] = 0.;
		if(total==0.) zcount++;
	}
	stats(nyrs,ydata,&qmean,&qmedian,&qstdv);
	MA[22] = qmean;
	if(usemedian) MA[22] = qmedian;
	LMA[22] = percentile(lb,nyrs,ydata);
	UMA[22] = percentile(ub,nyrs,ydata);
	if(LMA[22] > MA[22]) LMA[22] = MA[22];
	if(UMA[22] < MA[22]) UMA[22] = MA[22];

	stats(nyrs,stdv,&qmean,&dum,&qstdv);
	MA[34] = qmean;		// Coefficient of variation
	if(usemedian) MA[34] = dum;
	LMA[34] = percentile(lb,nyrs,stdv);
	UMA[34] = percentile(ub,nyrs,stdv);
	if(LMA[34] > MA[34]) LMA[34] = MA[34];
	if(UMA[34] < MA[34]) UMA[34] = MA[34];

// December
	for(i=0;i<nyrs;i++)
	{
		total = 0.;
		nmv = 0;
		for(j=61;j<92;j++)
		{
			if(q[i][j]  < nearhuge)
			{
				mdata[nmv] = q[i][j];
				total = total + mdata[nmv];
				nmv++;
			}
		}
		stats(nmv,mdata,&qmean,&dum,&qstdv);
		if(qmean != 0.)
			stdv[i] = 100*qstdv/qmean;
		else
			stdv[i] = 0.;
		if(nmv > 0)
			ydata[i] = total/nmv;
		else
			ydata[i] = 0.;
		if(total==0.) zcount++;
	}
	stats(nyrs,ydata,&qmean,&qmedian,&qstdv);
	MA[23] = qmean;
	if(usemedian) MA[23] = qmedian;
	LMA[23] = percentile(lb,nyrs,ydata);
	UMA[23] = percentile(ub,nyrs,ydata);
	if(LMA[23] > MA[23]) LMA[23] = MA[23];
	if(UMA[23] < MA[23]) UMA[23] = MA[23];

	stats(nyrs,stdv,&qmean,&dum,&qstdv);
	MA[35] = qmean;		// Coefficient of variation
	if(usemedian) MA[35] = dum;
	LMA[35] = percentile(lb,nyrs,stdv);
	UMA[35] = percentile(ub,nyrs,stdv);
	if(LMA[35] > MA[35]) LMA[35] = MA[35];
	if(UMA[35] < MA[35]) UMA[35] = MA[35];

// Compute variability across monthly flows 1

	double meanm[12][150];		// Mean flows for each month and year
	double minm[12][150];		// Minimum flows for each month and year
	double maxm[12][150];		// Maximum flows for each month and year
	int strt[12] = {0,31,61,92,123,152,183,213,244,274,305,336};
	int end[12] = {31,61,92,123,152,183,213,244,274,305,336,366};
	double maxqm, minqm;		// Max and min of the monthly means
	double q1, q3;			// First and third quartiles of the monthly means
	double q10, q90;			// Tenth and 90th percentiles of the monthly means
	int numvalidmonths;

// Compute the minimum, maximum, and mean flows for each month and year in the data set

    numvalidmonths = 0;

	for(i=0;i<nyrs;i++)
	{
		for(k=0;k<12;k++)
		{
			total = 0.;
			nmv = 0;
			minm[k][i] = nearhuge + 1.;
			maxm[k][i] = -nearhuge -1.;
            meanm[k][i]	= nearhuge + 1.;
			for(j=strt[k];j<end[k];j++)
			{
				if(q[i][j] < nearhuge)
				{
					total = total + q[i][j];
					nmv++;
					if(q[i][j] < minm[k][i]) minm[k][i] = q[i][j];
					if(q[i][j] > maxm[k][i]) maxm[k][i] = q[i][j];
				}
			}
			if (nmv > 0)
			{
			    numvalidmonths++;     // need to count number of valid months
    			meanm[k][i] = total / nmv;
			}
		}
	}

// Compute the statistics for the monthly means

	nmv = 0;
	maxqm = -10000000.;
	minqm = 10000000.;
	//tempdata = new double[numvalidmonths];  // create a new array for VALID months ONLY!
    double *tempdata = (double *) malloc(numvalidmonths * sizeof (double));
    if (tempdata == NULL)
    {
        printf("Could not allocate memory");
        exit(EXIT_FAILURE);
    }

// Transfer the monthly means to a single array and compute the max and min for the entire data set

	for(i=0;i<12;i++)
	{
		for(j=0;j<nyrs;j++)
		{
			if(meanm[i][j] < nearhuge)
			{
  			    tempdata[nmv] = meanm[i][j];
  			    if(tempdata[nmv] > maxqm) maxqm = tempdata[nmv];
  			    if(tempdata[nmv] < minqm) minqm = tempdata[nmv];
  			    nmv++;
			}
		}
	}

// Compute the statistics and percentiles

	stats(nmv,tempdata,&qmean,&qmedian,&qstdv);
	q1 = percentile(.25,nmv,tempdata);
	q3 = percentile(.75,nmv,tempdata);
	q10 = percentile(.10,nmv,tempdata);
	q90 = percentile(.90,nmv,tempdata);

// MA36		Variability across monthly flows 1.  Compute the minimum, maximum, and mean flows for each
//			month in the entire flow record.  Compute the first (25th percentile) and third (75th percentile)
//			quartiles and the 10th and 90th percentiles for the monthly means (every month in the flow
//			record).  MA36 is the maximum monthly flow minus the minimum monthly flow divided by the
//			median monthly flow.  (dimensionless)
//			Limits are not computed.

	MA[36] = 999999;
	if(qmedian != 0.) MA[36] = (maxqm - minqm)/qmedian;

// MA37		Variability across monthly flows 1.  MA37 is the third quartile minus the first quartile divided by
//			the median of the monthly means.  (dimensionless)
//			Limits are not computed.

	MA[37] = 999999;
	if(qmedian != 0.) MA[37] = (q3 - q1)/qmedian;

// MA38	Variability across monthly flows 1.  MA38 is the 90th percentile  minus the 10th percentile
//		divided by the median of the monthly means.  (dimensionless)
//		Limits are not computed.

	MA[38] = 999999;
	if(qmedian != 0.) MA[38] = (q90 - q10)/qmedian;

// Compute the variability across monthly flows 2  (coefficient of variation)

// MA39		Variability across monthly flows 2.  Compute the standard deviation for the monthly means.
//			MA39 is the standard deviation times 100 divided by the mean of the monthly means.  (%)
//			Limits are not computed.

	MA[39] = (qstdv/qmean)*100.;

// Compute the skewness in monthly flows

// MA40		Skewness in the monthly flows.  MA40 is the mean of the monthly flow means minus the
//			median of the monthly means divided by the median of the monthly means.  (dimensionless)
//			Limits are not computed.

	MA[40] = 999999;
	if(qmedian != 0.) MA[40] = (qmean - qmedian)/qmedian;

// Compute variability across annual flows and annual runoff

	double meana[150];		// Mean annual flows

// Compute the mean annual flows

	for(i=0;i<nyrs;i++)
	{
		nmv = 0;
		for(j=0;j<366;j++)
		{
			if(q[i][j] < nearhuge)
			{
				mdata[nmv] =  q[i][j];
				nmv++;
			}
		}
		stats(nmv,mdata,&qmean,&qmedian,&qstdv);
		meana[i] = qmean;
	}

// Compute the max and min of all mean annual flows

	nmv = 0;
	maxqm = -10000000.;
	minqm = 10000000.;
	for(i=0;i<nyrs;i++)
	{
		if(meana[i] > maxqm) maxqm = meana[i];
		if(meana[i] < minqm) minqm = meana[i];
	}

// Compute the statistics for the annual means and percentiles

	stats(nyrs,meana,&qmean,&qmedian,&qstdv);
	q1 = percentile(.25,nyrs,meana);
	q3 = percentile(.75,nyrs,meana);
	q10 = percentile(.10,nyrs,meana);
	q90 = percentile(.90,nyrs,meana);

// Compute the annual runoff

// MA41		Annual runoff.  Compute the annual mean flows.  MA41 is the mean of the annual means divided by the drainage area.
//		    (dimensionless)

	MA[41] = qmean / carea;
	temp = percentile(lb,nyrs,meana);
	LMA[41] = temp / carea;
	temp = percentile(ub,nyrs,meana);
	UMA[41] = temp / carea;
	if(LMA[41] > MA[41]) LMA[41] = MA[41];
	if(UMA[41] < MA[41]) UMA[41] = MA[41];

// Compute the variability across annual flows

// MA42		Variability across annual flows.  Compute the first (25th percentile) and third (75th percentile)
//			quartiles and the 10th and 90th percentiles for the annual means (every year in the flow
//			record).  MA42 is the maximum annual flow minus the minimum annual flow divided by the
//			median annual flow.  (dimensionless)
//			Limits are not computed.

	MA[42] = 999999;
	if(qmedian != 0.) MA[42] = (maxqm - minqm)/qmedian;
//    printf("maxqm: %12.4f\n",maxqm);
//   printf("minqm: %12.4f\n",minqm);
//    printf("qmean: %12.4f\n",qmean);
//    printf("qmedian: %12.4f\n",qmedian);

// MA43		Variability across annual flows.  MA43 is the third quartile minus the first quartile divided by
//			the median of the annual means.  (dimensionless)
//			Limits are not computed

	MA[43] = 999999;
	if(qmedian != 0.) MA[43] = (q3 - q1)/qmedian;

// MA44	Variability across monthly flows.  MA38 is the 90th percentile  minus the 10th percentile
//		divided by the median of the annual means.  (dimensionless)
//		Limits are not computed.

	MA[44] = 999999;
	if(qmedian != 0.) MA[44] = (q90 - q10)/qmedian;

// Compute the skewness in annual flows

// MA45		Skewness in the annual flows.  MA45 is the mean of the annual flow means minus the
//			median of the annual means divided by the median of the annual means.  (dimensionless)
//			Limits are not computed.

	MA[45] = 999999;
	if(qmedian != 0.) MA[45] = (qmean - qmedian)/qmedian;

//***************   Low flow conditions  **************************

/*	double ML1, LML1, UML1;	// January minimum monthly flows across all years
 *	double ML2, LML2, UML2;	// February minimum monthly flows across all years
 *	double ML3, LML3, UML3;	// March minimum monthly flows across all years
 *	double ML4, LML4, UML4;	// April minimum monthly flows across all years
 *	double ML5, LML5, UML5;	// May minimum monthly flows across all years
 *	double ML6, LML6, UML6;	// June minimum monthly flows across all years
 *	double ML7, LML7, UML7;	// July minimum monthly flows across all years
 *	double ML8, LML8, UML8;	// August minimum monthly flows across all years
 *	double ML9, LML9, UML9;	// September minimum monthly flows across all years
 *	double ML10, LML10, UML10;	// October minimum monthly flows across all years
 *	double ML11, LML11, UML11;	// November minimum monthly flows across all years
 *	double ML12, LML12, UML12;	// December minimum monthly flows across all years
 *	double ML13;		// Variability across minimum monthly flows
 *	double ML14, LML14, UML14;		// Mean of annual minimum flows
 *	double ML15, LML15, UML15;		// Low flow index
 *	double ML16, LML16, UML16;		// Median of annual minimum flows 2
 *	double ML17, LML17, UML17;		// Baseflow Index 1
 *	double ML18;		// Variability in Baseflow Index 1
 *	double ML19, LML19, UML19;		// Baseflow Index 2
 *	double ML20;		// Baseflow Index 3
 *	double ML21;		// Variability across annual minimum flows
 *	double ML22, LML22, UML22;		// Specific mean annual minimum flows
 * */
// Compute the mean minimum flows for each month across all years

// ML1 -		Mean minimum flows for each month across all years.  Compute the minumums for each month
//  ML12		over the entire flow record.  For example, ML1 is the mean of the minimums of all January flow
//				values over the entire record.  (cfs)

// January
    nmv = 0;
	for(i=0;i<nyrs;i++)
	{
		if(minm[3][i] < nearhuge)
		{
 		    mdata[nmv] = minm[3][i];
			nmv++;
        }
	}
	stats(nmv,mdata,&qmean,&qmedian,&qstdv);
	ML[1] = qmean;
	if(usemedian) ML[1] = qmedian;
	LML[1] = percentile(lb,nyrs,mdata);
	UML[1] = percentile(ub,nyrs,mdata);
	if(LML[1] > ML[1]) LML[1] = ML[1];
	if(UML[1] < ML[1]) UML[1] = ML[1];

// February
    nmv = 0;
	for(i=0;i<nyrs;i++)
	{
		if(minm[4][i] < nearhuge)
		{
 		    mdata[nmv] = minm[4][i];
			nmv++;
		}
	}
	stats(nmv,mdata,&qmean,&qmedian,&qstdv);
	ML[2] = qmean;
	if(usemedian) ML[2] = qmedian;
	LML[2] = percentile(lb,nyrs,mdata);
	UML[2] = percentile(ub,nyrs,mdata);
	if(LML[2] > ML[2]) LML[2] = ML[2];
	if(UML[2] < ML[2]) UML[2] = ML[2];

// March
    nmv = 0;
	for(i=0;i<nyrs;i++)
	{
		if(minm[5][i] < nearhuge)
		{
 		    mdata[nmv] = minm[5][i];
			nmv++;
		}
	}
	stats(nmv,mdata,&qmean,&qmedian,&qstdv);
	ML[3] = qmean;
	if(usemedian) ML[3] = qmedian;
	LML[3] = percentile(lb,nyrs,mdata);
	UML[3] = percentile(ub,nyrs,mdata);
	if(LML[3] > ML[3]) LML[3] = ML[3];
	if(UML[3] < ML[3]) UML[3] = ML[3];

// April
    nmv = 0;
	for(i=0;i<nyrs;i++)
	{
		if(minm[6][i] < nearhuge)
		{
 		    mdata[nmv] = minm[6][i];
			nmv++;
		}
	}
	stats(nmv,mdata,&qmean,&qmedian,&qstdv);
	ML[4] = qmean;
	if(usemedian) ML[4] = qmedian;
	LML[4] = percentile(lb,nyrs,mdata);
	UML[4] = percentile(ub,nyrs,mdata);
	if(LML[4] > ML[4]) LML[4] = ML[4];
	if(UML[4] < ML[4]) UML[4] = ML[4];

// May
    nmv = 0;
	for(i=0;i<nyrs;i++)
	{
		if(minm[7][i] < nearhuge)
		{
 		    mdata[nmv] = minm[7][i];
			nmv++;
		}
	}
	stats(nmv,mdata,&qmean,&qmedian,&qstdv);
	ML[5] = qmean;
	if(usemedian) ML[5] = qmedian;
	LML[5] = percentile(lb,nyrs,mdata);
	UML[5] = percentile(ub,nyrs,mdata);
	if(LML[5] > ML[5]) LML[5] = ML[5];
	if(UML[5] < ML[5]) UML[5] = ML[5];

// June
    nmv = 0;
	for(i=0;i<nyrs;i++)
	{
		if(minm[8][i] < nearhuge)
		{
 		    mdata[nmv] = minm[8][i];
			nmv++;
		}
	}
	stats(nmv,mdata,&qmean,&qmedian,&qstdv);
	ML[6] = qmean;
	if(usemedian) ML[6] = qmedian;
	LML[6] = percentile(lb,nyrs,mdata);
	UML[6] = percentile(ub,nyrs,mdata);
	if(LML[6] > ML[6]) LML[6] = ML[6];
	if(UML[6] < ML[6]) UML[6] = ML[6];

// July
    nmv = 0;
	for(i=0;i<nyrs;i++)
	{
		if(minm[9][i] < nearhuge)
		{
 		    mdata[nmv] = minm[9][i];
			nmv++;
		}
	}
	stats(nmv,mdata,&qmean,&qmedian,&qstdv);
	ML[7] = qmean;
	if(usemedian) ML[7] = qmedian;
	LML[7] = percentile(lb,nyrs,mdata);
	UML[7] = percentile(ub,nyrs,mdata);
	if(LML[7] > ML[7]) LML[7] = ML[7];
	if(UML[7] < ML[7]) UML[7] = ML[7];

// August
    nmv = 0;
	for(i=0;i<nyrs;i++)
	{
		if(minm[10][i] < nearhuge)
		{
 		    mdata[nmv] = minm[10][i];
			nmv++;
		}
	}
	stats(nmv,mdata,&qmean,&qmedian,&qstdv);
	ML[8] = qmean;
	if(usemedian) ML[8] = qmedian;
	LML[8] = percentile(lb,nyrs,mdata);
	UML[8] = percentile(ub,nyrs,mdata);
	if(LML[8] > ML[8]) LML[8] = ML[8];
	if(UML[8] < ML[8]) UML[8] = ML[8];

// September
    nmv = 0;
	for(i=0;i<nyrs;i++)
	{
		if(minm[11][i] < nearhuge)
		{
 		    mdata[nmv] = minm[11][i];
			nmv++;
		}
	}
	stats(nmv,mdata,&qmean,&qmedian,&qstdv);
	ML[9] = qmean;
	if(usemedian) ML[9] = qmedian;
	LML[9] = percentile(lb,nyrs,mdata);
	UML[9] = percentile(ub,nyrs,mdata);
	if(LML[9] > ML[9]) LML[9] = ML[9];
	if(UML[9] < ML[9]) UML[9] = ML[9];

// October
    nmv = 0;
	for(i=0;i<nyrs;i++)
	{
		if(minm[0][i] < nearhuge)
		{
 		    mdata[nmv] = minm[0][i];
			nmv++;
		}
	}
	stats(nmv,mdata,&qmean,&qmedian,&qstdv);
	ML[10] = qmean;
	if(usemedian) ML[10] = qmedian;
	LML[10] = percentile(lb,nyrs,mdata);
	UML[10] = percentile(ub,nyrs,mdata);
	if(LML[10] > ML[10]) LML[10] = ML[10];
	if(UML[10] < ML[10]) UML[10] = ML[10];

// November
    nmv = 0;
	for(i=0;i<nyrs;i++)
	{
		if(minm[1][i] < nearhuge)
		{
 		    mdata[nmv] = minm[1][i];
			nmv++;
		}
	}
	stats(nmv,mdata,&qmean,&qmedian,&qstdv);
	ML[11] = qmean;
	if(usemedian) ML[11] = qmedian;
	LML[11] = percentile(lb,nyrs,mdata);
	UML[11] = percentile(ub,nyrs,mdata);
	if(LML[11] > ML[11]) LML[11] = ML[11];
	if(UML[11] < ML[11]) UML[11] = ML[11];

// December
    nmv = 0;
	for(i=0;i<nyrs;i++)
	{
		if(minm[2][i] < nearhuge)
		{
 		    mdata[nmv] = minm[2][i];
			nmv++;
		}
	}
	stats(nmv,mdata,&qmean,&qmedian,&qstdv);
	ML[12] = qmean;
	if(usemedian) ML[12] = qmedian;
	LML[12] = percentile(lb,nyrs,mdata);
	UML[12] = percentile(ub,nyrs,mdata);
	if(LML[12] > ML[12]) LML[12] = ML[12];
	if(UML[12] < ML[12]) UML[12] = ML[12];


// Compute the variability across minimum monthly flows

	nmv = 0;
	for(i=0;i<12;i++)
	{
		for(j=0;j<nyrs;j++)
		{
			if(minm[i][j] < nearhuge)
			{
    			mdata[nmv] = minm[i][j];
	    		nmv++;
			}
		}
	}
	stats(nmv,mdata,&qmean,&qmedian,&qstdv);

// ML[13] 		Variability (coefficient of variation) across minimum monthly flow values.  Compute the
//				mean and standard deviation for the minimum monthly flows over the entire flow record.
//				ML[13] is the standard deviation times 100 divided by the mean minimum monthly flow for all
//				years.  (%)
//				Limits are not computed.

	ML[13] = 999999;
	if(qmean != 0.) ML[13] = (qstdv/qmean)*100.;

// Compute the minimum annual flows from the minimum monthly flows

	double minaq[150];		// Minumum annual flows
	double min_med;			// Median of annual minimum flows

	for(j=0;j<nyrs;j++)
	{
		minaq[j] = 10000000.;
		for(i=0;i<12;i++)
		{
			if(minm[i][j] < minaq[j]) minaq[j] = minm[i][j];
		}
	}

// Compute the mean and median flows for each year

	double total3[150];
	double mediana[150];
	total = total2 = 0.;
	for(i=0;i<nyrs;i++)
	{
		nmv = 0;
		for(j=0;j<366;j++)
		{
			if(q[i][j] < nearhuge)
			{
				mdata[nmv] = q[i][j];
				nmv++;
			}
		}
		stats(nmv,mdata,&qmean,&qmedian,&qstdv);
		if(qmean != 0.)
			ydata[i] = minaq[i]/qmean;
		else
			ydata[i] = 0.;
		if(qmedian != 0.)
			total3[i] = minaq[i]/qmedian;
		else
			total3[i] = 0.;
		mediana[i] = qmedian;
	}
	stats(nyrs,total3,&qmean,&qmedian,&qstdv);

// ML[14]		Compute the minimum annual flows for each year.  ML[14] is the mean of the ratios of minimum annual flows to the median flow for each
//			year.  (dimensionless)

	ML[14] = qmean;
	LML[14] = percentile(lb,nyrs,total3);
	UML[14] = percentile(ub,nyrs,total3);
	if(LML[14] > ML[14]) LML[14] = ML[14];
	if(UML[14] < ML[14]) UML[14] = ML[14];

// Compute the median of annual minimum flows 2

// ML[16]		Median of annual minimum flows 2.  ML[16] is the median of the ratios of minimum annual flows
//			to the median flow for each year.  (dimensionless)

	ML[16] = qmedian;
	LML[16] = LML[14];
	UML[16] = UML[14];
	if(LML[16] > ML[16]) LML[16] = ML[16];
	if(UML[16] < ML[16]) UML[16] = ML[16];

// Compute the low flow index

// ML[15]		Low flow index.  ML[15] is the mean of the ratios of minimum annual flows to the mean flow for
//			each year.  (dimensionless)

	stats(nyrs,ydata,&qmean,&qmedian,&qstdv);
	ML[15] = qmean;
	if(usemedian) ML[15] = qmedian;
	LML[15] = percentile(lb,nyrs,ydata);
	UML[15] = percentile(ub,nyrs,ydata);
	if(LML[15] > ML[15]) LML[15] = ML[15];
	if(UML[15] < ML[15]) UML[15] = ML[15];

// Recompute the mean annual flows

	for(i=0;i<nyrs;i++)
	{
		total = 0.;
		nmv = 0;
		for(j=0;j<366;j++)
		{
			if(q[i][j] < nearhuge)
			{
				total = total + q[i][j];
				nmv++;
			}
		}
		meana[i] = total / nmv;
	}

// Compute the Baseflow Index 1

// Compute the minimum 7 day average flow for each year divided by the mean annual flow

	for(i=0;i<nyrs;i++)
	{
		minqm = 10000000.;
		for(j=0;j<360;j++)
		{
			total = 0.;
			nmv = 0;
			for(k=j;k<j+7;k++)
			{
//				if(q[i][j] < nearhuge)
				if(q[i][k] < nearhuge)
				{
//					total = total + q[i][j];
					total = total + q[i][k];
					nmv++;
				}
			}
			mdata[i] = total / nmv;
			if(mdata[i] < minqm) minqm = mdata[i];
		}
		mdata[i] = 0.;
		if(meana[i] != 0.) mdata[i] = minqm / meana[i];
	}
	stats(nyrs,mdata,&qmean,&qmedian,&qstdv);

// ML[17]		Base flow index 1.  Compute the mean annual flows.  Compute the minimum of a 7-day moving
//			average flow for each year and divide them by the mean annual flow for that year.  ML[17] is
//			mean of those ratios.  (dimensionless)

	ML[17] = qmean;
	if(usemedian) ML[17] = qmedian;
	LML[17] = percentile(lb,nyrs,mdata);
	UML[17] = percentile(ub,nyrs,mdata);
	if(LML[17] > ML[17]) LML[17] = ML[17];
	if(UML[17] < ML[17]) UML[17] = ML[17];

// ML[18]		Variability in base flow index 1.  Compute the standard deviation for the ratios of
//			minimum 7-day moving average flows to mean annual flows for each year.  ML[18] is the
//			standard deviation times 100 divided by the mean of the ratios.  (%)
//			Limits are not computed.

	ML[18] = 999999;
	if(qmean != 0.) ML[18] = (qstdv/qmean)*100.;

// Compute the Baseflow Index 2

	total = 0.;
	for(i=0;i<nyrs;i++)
	{
		mdata[i] = 0.;
		if(meana[i] != 0.) mdata[i] = minaq[i]/meana[i];
	}
	stats(nyrs,mdata,&qmean,&qmedian,&qstdv);

// ML[19]		Base flow index 2.  Compute the ratios of the minimum annual flow to mean annual flow for
//			each year.  ML[19] is the mean of these ratios.  (dimensionless)

	ML[19] = 100*qmean;
	if(usemedian) ML[19] = 100*qmedian;
	LML[19] = 100*percentile(lb,nyrs,mdata);
	UML[19] = 100*percentile(ub,nyrs,mdata);
	if(LML[19] > ML[19]) LML[19] = ML[19];
	if(UML[19] < ML[19]) UML[19] = ML[19];

// Compute the Baseflow Index 3  (ML[20], Ratio of baseflow volume to total flow volume)

	ndv = 0;
	for(i=0;i<nyrs;i++)
	{
		for(j=0;j<366;j++)
		{
			if(q[i][j] < nearhuge && q[i][j] > 0)
			{
				data[ndv] = q[i][j];
				ndv++;
			}
		}
	}

// Divide the period into 5 day blocks

	nmv = ndv / 5;
	k = 0;
	for(i=0;i<nmv;i++)
	{
// Find the minimum flow for each 5 day block
		mdata[i] = 10000000.;
		for(j=0;j<5;j++)
		{
			if(data[k] < mdata[i]) mdata[i] = data[k];
			k++;
		}
	}

// Assign minimum flow as baseflow if .9 x min flow is less than the min flows for
// blocks on either side.

	for(i=0;i<nmv;i++)
	{
		baseflow[i] = 0.;
		if(i==0)
		{
			if(.9*mdata[i] < mdata[i+1]) baseflow[i] = mdata[i];
		}
		else if(i==nmv-1)
		{
			if(.9*mdata[i] < mdata[i-1]) baseflow[i] = mdata[i];
		}
		else
		{
			if(.9*mdata[i] < mdata[i+1] && .9*mdata[i] < mdata[i-1]) baseflow[i] = mdata[i];
		}
	}

// Fill in missing values using linear interpolation

	double incr;		// Linear interpolation increment
	int is, ie;		// Starting and ending index for missing values
	int nzv;		// Number of zero values

	is = ie = nzv = 0;

// Fill in any zeros on the ends of the data first

	for(i=0;i<nmv;i++)
	{
		if(baseflow[i] > 0.)
		{
			if(i > 0)
			{
				for(j=0;j<i;j++)
				{
					baseflow[j] = baseflow[i];
				}
				break;
			}
		}
	}
	for(i=nmv-1;i>=0;i--)
	{
		if(baseflow[i] > 0.)
		{
			if(i < nmv-1)
			{
				for(j=nmv-1;j>i;j--)
				{
					baseflow[j] = baseflow[i];
				}
				break;
			}
		}
	}

// Fill in the inside values

	for(i=0;i<nmv;i++)
	{
		if(baseflow[i] == 0. && is == 0)
		{
			is = ie = i;
			nzv = 1;
			continue;
		}
		if(baseflow[i] == 0. && is > 0)
		{
			ie = i;
			nzv++;
		}
		if(baseflow[i] > 0. && is > 0)
		{
			nzv++;
			incr = (baseflow[i] - baseflow[is-1])/nzv;
			for(j=is;j<=ie;j++)
			{
				baseflow[j] = baseflow[j-1] + incr;
			}
			is = ie = 0;
		}
	}

// Compute the total flow and total baseflow

	double tflow;		// Total flow
	double tbflow;		// Total base flow

	tflow = tbflow = 0.;
	for(i=0;i<ndv;i++){tflow = tflow + data[i];}
	for(i=0;i<nmv;i++)
	{
		for(j=5*i;j<5*i+5;j++)
		{
			tbflow = tbflow + baseflow[i];
		}
	}

// Fill in the remaining values

	for(i=j;i<ndv;i++){tbflow = tbflow + baseflow[nmv-1];}

// ML[20]		Base flow index 3.  Divide the data record into 5-day blocks after removing zero flow
//			values.  Find the minimum flow for each block.  Assign the minimum flow as a base flow
//			for that block if 90% of that minimum flow isless than the minimum flows for the blocks
//			on either side.  Otherwise set it to zero.  Fill in the zero values using linear
//			interpolation.  Compute the total flow for the entire record and the total
//			base flow for the entire record.  ML[20] is the ratio of total base flow to total flow.
//			(dimensionless)
//			Limits are not computed.

	ML[20] = 999999;
	if(tflow > 0.) ML[20] = tbflow / tflow;

// Compute the variability across annual minimum flows

	stats(nyrs,minaq,&qmean,&qmedian,&qstdv);
	min_med = qmedian;

// ML[21]		Variability across annual minimum flows.  Compute the mean and standard deviation for the
//			annual minimum flows.  ML[21] is the standard deviation times 100 divided by the mean.  (%)
//			Limits are not computed.

	ML[21] = 999999;
	if(qmean != 0.) ML[21] = (qstdv/qmean)*100.;

// Compute the specific mean annual minimum flow

// ML[22]		Specific mean annual minimum flow.  ML[22] is the mean of the annual minimum flows
//			divided by the drainage area.  (cfs/mi2)

	ML[22] = qmean / carea;
	if(usemedian) ML[22] = qmedian / carea;
	temp = percentile(lb,nyrs,minaq);
	LML[22] = temp / carea;
	temp = percentile(ub,nyrs,minaq);
	UML[22] = temp / carea;
	if(LML[22] > ML[22]) LML[22] = ML[22];
	if(UML[22] < ML[22]) UML[22] = ML[22];

//***************   High flow conditions  **************************

/*	double MH1, LMH1, UMH1;	// January maximum monthly flows across all years
 *	double MH2, LMH2, UMH2;	// February maximum monthly flows across all years
 *	double MH3, LMH3, UMH3;	// March maximum monthly flows across all years
 *	double MH4, LMH4, UMH4;	// April maximum monthly flows across all years
 *	double MH5, LMH5, UMH5;	// May maximum monthly flows across all years
 *	double MH6, LMH6, UMH6;	// June maximum monthly flows across all years
 *	double MH7, LMH7, UMH7;	// July maximum monthly flows across all years
 *	double MH8, LMH8, UMH8;	// August maximum monthly flows across all years
 *	double MH9, LMH9, UMH9;	// September maximum monthly flows across all years
 *	double MH10, LMH10, UMH10;	// October maximum monthly flows across all years
 *	double MH11, LMH11, UMH11;	// November maximum monthly flows across all years
 *	double MH12, LMH12, UMH12;	// December maximum monthly flows across all years
 *	double MH13;		// Variability across maximum monthly flows
 *	double MH14, LMH14, UMH14;		// Median of annual minimum flows
 *	double MH15, MH16, MH17;		// High flow discharge metrics
 *	double MH18;		// Variability across annual maximum flows
 *	double MH19;		// Skewness in annual maximum flows
 *	double MH20, LMH20, UMH20;		// Specific mean annual maximum flows
 *	double MH21, LMH21, UMH21;		// High flow volume metrics
 *	double MH22, LMH22, UMH22;		// High flow volume metrics
 *	double MH23, LMH23, UMH23;		// High flow volume metrics
 *	double MH24, LMH24, UMH24;		// High peak flow 1 metrics
 *	double MH25, LMH25, UMH25;		// High peak flow 1 metrics
 *	double MH26, LMH26, UMH26;		// High peak flow 1 metrics
 *	double MH27, LMH27, UMH27;		// High peak flow 2 metric */

// Compute the mean maximum flows for each month across all years

// MH[1] -		Mean maximum flows for each month across all years.  Compute the maximums for each month
//  MH[12]		over the entire flow record.  For example, MH[1] is the mean of the maximums of all January flow
//				values over the entire record.  (cfs)

// January

    nmv = 0;
	for(i=0;i<nyrs;i++)
	{
		if(maxm[3][i] > -nearhuge)
		{
 		    mdata[nmv] = maxm[3][i];
			nmv++;
		}
	}
	stats(nmv,mdata,&qmean,&qmedian,&qstdv);
	MH[1] = qmean;
	if(usemedian) MH[1] = qmedian;
	LMH[1] = percentile(lb,nyrs,mdata);
	UMH[1] = percentile(ub,nyrs,mdata);
	if(LMH[1] > MH[1]) LMH[1] = MH[1];
	if(UMH[1] < MH[1]) UMH[1] = MH[1];

// February

    nmv = 0;
	for(i=0;i<nyrs;i++)
	{
		if(maxm[4][i] > -nearhuge)
		{
 		    mdata[nmv] = maxm[4][i];
			nmv++;
		}
	}
	stats(nmv,mdata,&qmean,&qmedian,&qstdv);
	MH[2] = qmean;
	if(usemedian) MH[2] = qmedian;
	LMH[2] = percentile(lb,nyrs,mdata);
	UMH[2] = percentile(ub,nyrs,mdata);
	if(LMH[2] > MH[2]) LMH[2] = MH[2];
	if(UMH[2] < MH[2]) UMH[2] = MH[2];

// March

    nmv = 0;
	for(i=0;i<nyrs;i++)
	{
		if(maxm[5][i] > -nearhuge)
		{
 		    mdata[nmv] = maxm[5][i];
			nmv++;
		}
	}
	stats(nmv,mdata,&qmean,&qmedian,&qstdv);
	MH[3] = qmean;
	if(usemedian) MH[3] = qmedian;
	LMH[3] = percentile(lb,nyrs,mdata);
	UMH[3] = percentile(ub,nyrs,mdata);
	if(LMH[3] > MH[3]) LMH[3] = MH[3];
	if(UMH[3] < MH[3]) UMH[3] = MH[3];

// April

    nmv = 0;
	for(i=0;i<nyrs;i++)
	{
		if(maxm[6][i] > -nearhuge)
		{
 		    mdata[nmv] = maxm[6][i];
			nmv++;
		}
	}
	stats(nmv,mdata,&qmean,&qmedian,&qstdv);
	MH[4] = qmean;
	if(usemedian) MH[4] = qmedian;
	LMH[4] = percentile(lb,nyrs,mdata);
	UMH[4] = percentile(ub,nyrs,mdata);
	if(LMH[4] > MH[4]) LMH[4] = MH[4];
	if(UMH[4] < MH[4]) UMH[4] = MH[4];

// May

    nmv = 0;
	for(i=0;i<nyrs;i++)
	{
		if(maxm[7][i] > -nearhuge)
		{
 		    mdata[nmv] = maxm[7][i];
			nmv++;
		}
	}
	stats(nmv,mdata,&qmean,&qmedian,&qstdv);
	MH[5] = qmean;
	if(usemedian) MH[5] = qmedian;
	LMH[5] = percentile(lb,nyrs,mdata);
	UMH[5] = percentile(ub,nyrs,mdata);
	if(LMH[5] > MH[5]) LMH[5] = MH[5];
	if(UMH[5] < MH[5]) UMH[5] = MH[5];

// June

    nmv = 0;
	for(i=0;i<nyrs;i++)
	{
		if(maxm[8][i] > -nearhuge)
		{
 		    mdata[nmv] = maxm[8][i];
			nmv++;
		}
	}
	stats(nmv,mdata,&qmean,&qmedian,&qstdv);
	MH[6] = qmean;
	if(usemedian) MH[6] = qmedian;
	LMH[6] = percentile(lb,nyrs,mdata);
	UMH[6] = percentile(ub,nyrs,mdata);
	if(LMH[6] > MH[6]) LMH[6] = MH[6];
	if(UMH[6] < MH[6]) UMH[6] = MH[6];

// July

    nmv = 0;
	for(i=0;i<nyrs;i++)
	{
		if(maxm[9][i] > -nearhuge)
		{
 		    mdata[nmv] = maxm[9][i];
			nmv++;
		}
	}
	stats(nmv,mdata,&qmean,&qmedian,&qstdv);
	MH[7] = qmean;
	if(usemedian) MH[7] = qmedian;
	LMH[7] = percentile(lb,nyrs,mdata);
	UMH[7] = percentile(ub,nyrs,mdata);
	if(LMH[7] > MH[7]) LMH[7] = MH[7];
	if(UMH[7] < MH[7]) UMH[7] = MH[7];

// August

    nmv = 0;
	for(i=0;i<nyrs;i++)
	{
		if(maxm[10][i] > -nearhuge)
		{
 		    mdata[nmv] = maxm[10][i];
			nmv++;
		}
	}
	stats(nmv,mdata,&qmean,&qmedian,&qstdv);
	MH[8] = qmean;
	if(usemedian) MH[8] = qmedian;
	LMH[8] = percentile(lb,nyrs,mdata);
	UMH[8] = percentile(ub,nyrs,mdata);
	if(LMH[8] > MH[8]) LMH[8] = MH[8];
	if(UMH[8] < MH[8]) UMH[8] = MH[8];

// September

    nmv = 0;
	for(i=0;i<nyrs;i++)
	{
		if(maxm[11][i] > -nearhuge)
		{
 		    mdata[nmv] = maxm[11][i];
			nmv++;
		}
	}
	stats(nmv,mdata,&qmean,&qmedian,&qstdv);
	MH[9] = qmean;
	if(usemedian) MH[9] = qmedian;
	LMH[9] = percentile(lb,nyrs,mdata);
	UMH[9] = percentile(ub,nyrs,mdata);
	if(LMH[9] > MH[9]) LMH[9] = MH[9];
	if(UMH[9] < MH[9]) UMH[9] = MH[9];

// October

    nmv = 0;
	for(i=0;i<nyrs;i++)
	{
		if(maxm[0][i] > -nearhuge)
		{
 		    mdata[nmv] = maxm[0][i];
			nmv++;
		}
	}
	stats(nmv,mdata,&qmean,&qmedian,&qstdv);
	MH[10] = qmean;
	if(usemedian) MH[10] = qmedian;
	LMH[10] = percentile(lb,nyrs,mdata);
	UMH[10] = percentile(ub,nyrs,mdata);
	if(LMH[10] > MH[10]) LMH[10] = MH[10];
	if(UMH[10] < MH[10]) UMH[10] = MH[10];

// November

    nmv = 0;
	for(i=0;i<nyrs;i++)
	{
		if(maxm[1][i] > -nearhuge)
		{
 		    mdata[nmv] = maxm[1][i];
			nmv++;
		}
	}
	stats(nmv,mdata,&qmean,&qmedian,&qstdv);
	MH[11] = qmean;
	if(usemedian) MH[11] = qmedian;
	LMH[11] = percentile(lb,nyrs,mdata);
	UMH[11] = percentile(ub,nyrs,mdata);
	if(LMH[11] > MH[11]) LMH[11] = MH[11];
	if(UMH[11] < MH[11]) UMH[11] = MH[11];

// December

    nmv = 0;
	for(i=0;i<nyrs;i++)
	{
		if(maxm[2][i] > -nearhuge)
		{
 		    mdata[nmv] = maxm[2][i];
			nmv++;
		}
	}
	stats(nmv,mdata,&qmean,&qmedian,&qstdv);
	MH[12] = qmean;
	if(usemedian) MH[12] = qmedian;
	LMH[12] = percentile(lb,nyrs,mdata);
	UMH[12] = percentile(ub,nyrs,mdata);
	if(LMH[12] > MH[12]) LMH[12] = MH[12];
	if(UMH[12] < MH[12]) UMH[12] = MH[12];

// Compute the variability across maximum monthly flows

	nmv = 0;
	for(i=0;i<12;i++)
	{
		for(j=0;j<nyrs;j++)
		{
			if(maxm[i][j] > -nearhuge)
			{
			    mdata[nmv] = maxm[i][j];
			    nmv++;
			}
		}
	}
	stats(nmv,mdata,&qmean,&qmedian,&qstdv);

// MH[13] 	Variability (coefficient of variation) across maximum monthly flow values.  Compute the
//			mean and standard deviation for the maximum monthly flows over the entire flow record.
//			MH[13] is the standard deviation times 100 divided by the mean minimum monthly flow for all
//			years.  (%)
//			Limits are not computed.

	MH[13] = 999999;
	if(qmean != 0.) MH[13] = (qstdv/qmean)*100.;

// Compute the maximum annual flows from the maximum monthly flows

	double maxaq[150];		// Maxumum annual flows

	for(j=0;j<nyrs;j++)
	{
		maxaq[j] = -10000000.;
		for(i=0;i<12;i++)
		{
			if(maxm[i][j] > maxaq[j]) maxaq[j] = maxm[i][j];
		}
	}
	for(i=0;i<nyrs;i++)
	{
		nmv = 0;
		for(j=0;j<366;j++)
		{
			if(q[i][j] < nearhuge)
			{
				mdata[nmv] = q[i][j];
				nmv++;
			}
		}
		stats(nmv,mdata,&qmean,&qmedian,&qstdv);
		total3[i] = 0.;
		if(qmedian != 0.) total3[i] = maxaq[i]/qmedian;
	}

// Compute the median of annual maximum flows

// MH[14]		Median of annual maximum flows.  Compute the annual maximum flows from monthly
//			maximum flows.  Compute the ratio of annual maximum flow to median annual flow for each
//			year.  MH[14] is the median of these ratios.  (dimensionless)

	stats(nyrs,total3,&qmean,&qmedian,&qstdv);
	MH[14] = qmedian;
	LMH[14] = percentile(lb,nyrs,total3);
	UMH[14] = percentile(ub,nyrs,total3);
	if(LMH[14] > MH[14]) LMH[14] = MH[14];
	if(UMH[14] < MH[14]) UMH[14] = MH[14];

// Compute the high flow discharge metrics

	ndv = 0;		// Reread the data array
	for(i=0;i<nyrs;i++)
	{
		for(j=0;j<366;j++)
		{
			if(q[i][j]  < nearhuge)
			{
				data[ndv] = q[i][j];
				ndv++;
			}
		}
	}

// Sort the flow data in descending order

	order(0,data,ndv);

// MH[15]		High flow discharge metric 1.  Compute the 1% exceedence value for the entire data record.
//			MH[15] is the 1% exceedence value divided by the median flow for the entire record.
//			(dimensionless)
//			Limits are not computed.

	temp = percentile(.01,ndv,data);
	MH[15] = 999999;
	if(MA[2] != 0.) MH[15] = temp / MA[2];

// MH[16]		High flow discharge metric 2.  Compute the 10% exceedence value for the entire data record.
//			MH[16] is the 10% exceedence value divided by the median flow for the entire record.
//			(dimensionless)
//			Limits are not computed.

	temp = percentile(.10,ndv,data);
	MH[16] = 999999;
	if(MA[2] != 0.) MH[16] = temp / MA[2];

// MH[17]		High flow discharge metric 3.  Compute the 25% exceedence value for the entire data record.
//			MH[17] is the 25% exceedence value divided by the median flow for the entire record.
//			(dimensionless)
//			Limits are not computed.

	temp = percentile(.25,ndv,data);
	MH[17] = 999999;
	if(MA[2] != 0.) MH[17] = temp / MA[2];

// Compute variability across annual maximum flows

	double lmaxaq[150];		// Log10 of annual maximums
	for(i=0;i<nyrs;i++)
	{
		if(maxaq[i] != 0.)
			lmaxaq[i] = (double)log10((double)maxaq[i]);
		else
			lmaxaq[i] = (double)log10(.01);
	}
	stats(nyrs,lmaxaq,&qmean,&qmedian,&qstdv);

// MH[18]		Variability across annual maximum flows.  Compute the logs (log10) of the maximum annual
//			flows.  Find the standard deviation and mean for these values.  MH[18] is the standard deviation
//			times 100 divided by the mean.  (%)
//			Limits are not computed.

	MH[18] = 999999;
	if(qmean != 0.) MH[18] = (qstdv/qmean)*100.;


// Compute the skewness in annual maximum flows  (MH[19])

// MH[19]		Skewness in annual maximum flows.  Use the equation:

//    MH[19] = N2 x sum(qm3)-3N x sum(qm) x sum(qm2) + 2 x (sum(qm))3 / N x (N-1) x (N-2) x stddev3

//	where:
//		N   	= Number of years
//		qm	= Log10(annual maximum flows)
//		stddev 	= Standard deviation of the annual maximum flows

//		(dimensionless)
//		Limits are not computed.

	double sumlq3;		// Sum of the log max flows cubed
	double sumlq2;		// Sum of the log max flows squared
	double sumlq;		// Sum of the log max flows
	double n;			// Number of years

	sumlq3 = sumlq2 = sumlq = 0.;
	n = (double)nyrs;
	for(i=0;i<nyrs;i++)
	{
		sumlq3 = sumlq3 + lmaxaq[i]*lmaxaq[i]*lmaxaq[i];
		sumlq2 = sumlq2 + lmaxaq[i]*lmaxaq[i];
		sumlq = sumlq + lmaxaq[i];
	}
	stats(nyrs,lmaxaq,&qmean,&qmedian,&qstdv);
	MH[19] = 999999;
	if(qstdv != 0.)
		MH[19] = (n*n*sumlq3 - 3*n*sumlq*sumlq2 + 2*sumlq*sumlq*sumlq)/(n*(n-1)*(n-2)*qstdv*qstdv*qstdv);

// Compute the specific mean annual maximum flow

	stats(nyrs,maxaq,&qmean,&qmedian,&qstdv);

// MH[20]		Specific mean annual maximum flow.  MH[20] is the mean of the annual maximum flows
//			divided by the drainage area.  (cfs/mi2)

	MH[20] = qmean / carea;
	if(usemedian) MH[20] = qmedian / carea;
	temp = percentile(lb,nyrs,maxaq);
	LMH[20] = temp / carea;
	temp = percentile(ub,nyrs,maxaq);
	UMH[20] = temp / carea;
	if(LMH[20] > MH[20]) LMH[20] = MH[20];
	if(UMH[20] < MH[20]) UMH[20] = MH[20];

// Compute the high volume flow metrics

	double threshold;	// Threshold for computing volumes
	int nevents;		// Number of events
	int flag;			// Indicates pulse is occuring

	threshold = MA[2];
	nmv = 0;
	for(i=0;i<nyrs;i++)
	{
		flag = 0;
		nevents = 0;
		total = 0.;
		for(j=0;j<366;j++)
		{
			if(q[i][j] < nearhuge)
			{
				temp = q[i][j] - threshold;
				if(temp > 0.)
				{
					total = total + temp;
					flag++;
					if(flag==1) nevents++;
				}
				else
				{
					flag = 0;
				}
			}
		}
		if(nevents > 0)
		{
			total3[nmv] = nevents;
			ydata[nmv] = total;
			nmv++;
		}
	}
	stats(nmv,ydata,&qmean,&qmedian,&qstdv);
	stats(nmv,total3,&temp,&qmedian,&qstdv);

// MH[21]		High flow volume 1.  Compute the average duration for flow events above a threshold equal to
//			the median flow for the entire record.  MH[21] is the average duration divided by the median flow
//			for the entire record.  (days/cfs)

	MH[21] = 999999;
	if(temp != 0.) MH[21] = qmean / temp;
	if(MA[2] != 0 && MH[21] != 999999)
		MH[21] = MH[21] / MA[2];
	else
		MH[21] = 999999;
	total = percentile(lb,nmv,ydata);
	total2 = percentile(lb,nmv,total3);
	LMH[21] = 999999;
	if(total2 != 0.) LMH[21] = total/total2;
	if(MA[2] != 0 && LMH[21] != 999999)
		LMH[21] = LMH[21] / MA[2];
	else
		LMH[21] = 999999;
	total = percentile(ub,nmv,ydata);
	total2 = percentile(ub,nmv,total3);
	UMH[21] = 999999;
	if(total2 != 0.) UMH[21] = total/total2;
	if(MA[2] != 0 && UMH[21] != 999999)
		UMH[21] = UMH[21] / MA[2];
	else
		UMH[21] = 999999;
	if(LMH[21] > MH[21]) LMH[21] = MH[21];
	if(UMH[21] < MH[21]) UMH[21] = MH[21];

	threshold = 3.*MA[2];
	nmv = 0;
	for(i=0;i<nyrs;i++)
	{
		flag = 0;
		nevents = 0;
		total = 0.;
		for(j=0;j<366;j++)
		{
			if(q[i][j] < nearhuge)
			{
				temp = q[i][j] - threshold;
				if(temp > 0.)
				{
					total = total + temp;
					flag++;
					if(flag==1) nevents++;
				}
				else
				{
					flag = 0;
				}
			}
		}
		if(nevents > 0)
		{
			total3[nmv] = nevents;
			ydata[nmv] = total;
			nmv++;
		}
	}
	stats(nmv,ydata,&qmean,&qmedian,&qstdv);
	stats(nmv,total3,&temp,&qmedian,&qstdv);

// MH[22]		High flow volume 1.  Compute the average duration for flow events above a threshold equal to
//			three times the median flow for the entire record.  MH[22] is the average duration divided by the
//			median flow for the entire record.  (days/cfs)

	MH[22] = 999999;
	if(temp != 0.) MH[22] = qmean / temp;
	if(MA[2] != 0 && MH[22] != 999999)
		MH[22] = MH[22] / MA[2];
	else
		MH[22] = 999999;
	total = percentile(lb,nmv,ydata);
	total2 = percentile(lb,nmv,total3);
	LMH[22] = 999999;
	if(total2 != 0.) LMH[22] = total/total2;
	if(MA[2] != 0 && LMH[22] != 999999)
		LMH[22] = LMH[22] / MA[2];
	else
		LMH[22] = 999999;
	total = percentile(ub,nmv,ydata);
	total2 = percentile(ub,nmv,total3);
	UMH[22] = 999999;
	if(total2 != 0.) UMH[22] = total/total2;
	if(MA[2] != 0 && UMH[22] != 999999)
		UMH[22] = UMH[22] / MA[2];
	else
		UMH[22] = 999999;
	if(LMH[22] > MH[22]) LMH[22] = MH[22];
	if(UMH[22] < MH[22]) UMH[22] = MH[22];

	threshold = 7.*MA[2];
	nmv = 0;
	for(i=0;i<nyrs;i++)
	{
		flag = 0;
		nevents = 0;
		total = 0.;
		for(j=0;j<366;j++)
		{
			if(q[i][j] < nearhuge)
			{
				temp = q[i][j] - threshold;
				if(temp > 0.)
				{
					total = total + temp;
					flag++;
					if(flag==1) nevents++;
				}
				else
				{
					flag = 0;
				}
			}
		}
		if(nevents > 0)
		{
			total3[nmv] = nevents;
			ydata[nmv] = total;
			nmv++;
		}
	}
	stats(nmv,ydata,&qmean,&qmedian,&qstdv);
	stats(nmv,total3,&temp,&qmedian,&qstdv);

// MH[23]		High flow volume 1.  Compute the average duration for flow events above a threshold equal to
//			seven times the median flow for the entire record.  MH[23] is the average duration divided by the
//			median flow for the entire record.  (days/cfs)

	MH[23] = 999999;
	if(temp != 0.) MH[23] = qmean / temp;
	if(MA[2] != 0 && MH[23] != 999999)
		MH[23] = MH[23] / MA[2];
	else
		MH[23] = 999999;
	total = percentile(lb,nmv,ydata);
	total2 = percentile(lb,nmv,total3);
	LMH[23] = 999999;
	if(total2 != 0.) LMH[23] = total/total2;
	if(MA[2] != 0 && LMH[23] != 999999)
		LMH[23] = LMH[23] / MA[2];
	else
		LMH[23] = 999999;
	total = percentile(ub,nmv,ydata);
	total2 = percentile(ub,nmv,total3);
	UMH[23] = 999999;
	if(total2 != 0.) UMH[23] = total/total2;
	if(MA[2] != 0 && UMH[21] != 999999)
		UMH[23] = UMH[23] / MA[2];
	else
		UMH[23] = 999999;
	if(LMH[23] > MH[23]) LMH[23] = MH[23];
	if(UMH[23] < MH[23]) UMH[23] = MH[23];

// Compute high peak flow 1 metrics

	threshold = MA[2];
	nevents = 0;
	for(i=0;i<nyrs;i++)
	{
		flag = 0;
		for(j=0;j<366;j++)
		{
			if(q[i][j] < nearhuge)
			{
				temp = q[i][j];
				if(temp > threshold)
				{
					if(flag == 0)mdata[nevents] = temp;
					if(flag > 0 && temp > mdata[nevents-1])mdata[nevents-1] = temp;
					flag++;
					if(flag==1)
					{
						nevents++;
					}
				}
				else
				{
					flag = 0;
				}
			}
		}
	}
	stats(nevents,mdata,&qmean,&qmedian,&qstdv);

// MH[24]		High peak flow 1.  Compute the average peak flow value for flow events above a threshold equal
 //			to the median flow for the entire record.  MH[24] is the average duration divided by the median
//			flow for the entire record.  (days/cfs)

	MH[24] = LMH[24] = UMH[24] = 999999;
	if(MA[2] != 0.) MH[24] = qmean/MA[2];
	temp = percentile(lb,nevents,mdata);
	if(MA[2] != 0.) LMH[24] = temp / MA[2];
	temp = percentile(ub,nevents,mdata);
	if(MA[2] != 0.) UMH[24] = temp / MA[2];
	if(LMH[24] > MH[24]) LMH[24] = MH[24];
	if(UMH[24] < MH[24]) UMH[24] = MH[24];


	threshold = 3.*MA[2];
	nevents = 0;
	for(i=0;i<nyrs;i++)
	{
		flag = 0;
		for(j=0;j<366;j++)
		{
			if(q[i][j] < nearhuge)
			{
				temp = q[i][j];
				if(temp > threshold)
				{
					if(flag == 0)mdata[nevents] = temp;
					if(flag > 0 && temp > mdata[nevents-1])mdata[nevents-1] = temp;
					flag++;
					if(flag==1)
					{
						nevents++;
					}
				}
				else
				{
					flag = 0;
				}
			}
		}
	}
	stats(nevents,mdata,&qmean,&qmedian,&qstdv);

// MH[25]		High peak flow 1.  Compute the average peak flow value for flow events above a threshold equal
//			to three times the median flow for the entire record.  MH[25] is the average duration divided by
//			the median flow for the entire record.  (days/cfs)

	MH[25] = LMH[25] = UMH[25] = 999999;
	if(MA[2] != 0.) MH[25] = qmean/MA[2];
	temp = percentile(lb,nevents,mdata);
	if(MA[2] != 0.) LMH[25] = temp / MA[2];
	temp = percentile(ub,nevents,mdata);
	if(MA[2] != 0.) UMH[25] = temp / MA[2];
	if(LMH[25] > MH[25]) LMH[25] = MH[25];
	if(UMH[25] < MH[25]) UMH[25] = MH[25];

	threshold = 7.*MA[2];
	nevents = 0;
	for(i=0;i<nyrs;i++)
	{
		flag = 0;
		for(j=0;j<366;j++)
		{
			if(q[i][j] < nearhuge)
			{
				temp = q[i][j];
				if(temp > threshold)
				{
					if(flag == 0)mdata[nevents] = temp;
					if(flag > 0 && temp > mdata[nevents-1])mdata[nevents-1] = temp;
					flag++;
					if(flag==1)
					{
						nevents++;
					}
				}
				else
				{
					flag = 0;
				}
			}
		}
	}
	stats(nevents,mdata,&qmean,&qmedian,&qstdv);

// MH[26]		High peak flow 1.  Compute the average peak flow value for flow events above a threshold equal
//			to seven times the median flow for the entire record.  MH[26] is the average duration divided by
//			the median flow for the entire record.  (days/cfs)

	MH[26] = LMH[26] = UMH[26] = 999999;
	if(MA[2] != 0.) MH[26] = qmean/MA[2];
	temp = percentile(lb,nevents,mdata);
	if(MA[2] != 0.) LMH[26] = temp / MA[2];
	temp = percentile(ub,nevents,mdata);
	if(MA[2] != 0.) UMH[26] = temp / MA[2];
	if(LMH[26] > MH[26]) LMH[26] = MH[26];
	if(UMH[26] < MH[26]) UMH[26] = MH[26];

// Compute the high peak flow 2 metric

	order(1,data,ndv);		// Order data in ascending order
	threshold = percentile(.75,ndv,data);
	nevents = 0;
	for(i=0;i<nyrs;i++)
	{
		flag = 0;
		for(j=0;j<366;j++)
		{
			if(q[i][j] < nearhuge)
			{
				temp = q[i][j];
				if(temp > threshold)
				{
					if(flag == 0)mdata[nevents] = temp;
					if(flag > 0 && temp > mdata[nevents-1])mdata[nevents-1] = temp;
					flag++;
					if(flag==1)
					{
						nevents++;
					}
				}
				else
				{
					flag = 0;
				}
			}
		}
	}
	stats(nevents,mdata,&qmean,&qmedian,&qstdv);

// MH[27]		High peak flow 2.  Compute the average peak flow value for flow events above a threshold equal
//			to 75th percentile value for the entire flow record.  MH[27] is the average duration divided by the
//			median flow for the entire record.  (days/cfs)

	MH[27] = LMH[27] = UMH[27] = 999999;
	if(MA[2] != 0.) MH[27] = qmean/MA[2];
	temp = percentile(lb,nevents,mdata);
	if(MA[2] != 0.) LMH[27] = temp / MA[2];
	temp = percentile(ub,nevents,mdata);
	if(MA[2] != 0.) UMH[27] = temp / MA[2];
	if(LMH[27] > MH[27]) LMH[27] = MH[27];
	if(UMH[27] < MH[27]) UMH[27] = MH[27];

//******************  Frequency of flow events  ********************************

//******************  Low Flow Conditions  *************************************

/*	double FL1, LFL1, UFL1;		// Low flood pulse count
 *	double FL2;					// Variability in low flow pulse count
 *	double FL3, LFL3, UFL3;		// Frequency of low flow spells */

	double np[150];		// Number of pulse occurrences for each year
	double p25;			// 25th percentile flow
	double p5;			// 5% of mean annual flow
	int pdur;			// Pulse duration
	double lfdur[150];	// Average low flow pulse duration for each year
	double hfdur[150];	// Average high flow pulse duration for each year

	p25 = percentile(.25,ndv,data);
	p5 = .05 * MA[1];
	for(i=0;i<nyrs;i++)
	{
		np[i] = 0.;			// Number of events or pulses per year
		lfdur[i] = 0.;		// Average duration of pulses for a year (days)
		pdur = flag = 0;
		for(j=0;j<366;j++)
		{
			if(q[i][j] >= nearhuge) continue;
			if(q[i][j] < p25)
			{
				pdur++;		// Increment days in the pulse
				flag++;
				if(flag==1) np[i]++;	// Add another pulse
			}
			else
			{
				flag = 0;
			}
		}
		if(np[i]>0.) lfdur[i] = pdur / np[i];	// Compute the average duration
	}
	stats(nyrs,np,&qmean,&qmedian,&qstdv);

// FL[1]		Low flood pulse count.  Compute the average number of flow events with flows below a
//			threshold equal to the 25th percentile value for the entire flow record.  FL[1] is the average
//			number of events.  (number of events)

	FL[1] = qmean;
	if(usemedian) FL[1] = qmedian;
	LFL[1] = percentile(lb,nyrs,np);
	UFL[1] = percentile(ub,nyrs,np);
	if(LFL[1] > FL[1]) LFL[1] = FL[1];
	if(UFL[1] < FL[1]) UFL[1] = FL[1];

// FL[2]		Variability in low pulse count.  Compute the standard deviation in the annual pulse counts for
//			FL[1].  FL[2] is 100 times the standard deviation divided by the mean pulse count (FL[1]).  (%)
//			Limits are not computed.

	if(FL[1] != 0.)
		FL[2] = 100*qstdv / FL[1];
	else
		FL[2] = 999999;

	for(i=0;i<nyrs;i++)
	{
		np[i] = 0.;		// Number of pulses per year
		for(j=0;j<366;j++)
		{
			if(q[i][j] >= nearhuge) continue;
			if(q[i][j] < p5)
			{
				np[i]++;
//    			printf("YEAR: %i  DAY: %i  VALUE: %12.3f  PULSE No.: %i\n", i,j,q[i][j],np[i]);
			}
		}
	}
	stats(nyrs,np,&qmean,&qmedian,&qstdv);

// FL[3]		Frequency of low pulse spells.  Compute the average number of flow events with flows below a
//			threshold equal to 5% of the mean flow value for the entire flow record.  FL[3] is the average
//			number of events.  (number of events)

	FL[3] = qmean;
	if(usemedian) FL[3] = qmedian;
	LFL[3] = percentile(lb,nyrs,np);
	UFL[3] = percentile(ub,nyrs,np);
	if(LFL[3] > FL[3]) LFL[3] = FL[3];
	if(UFL[3] < FL[3]) UFL[3] = FL[3];

//			printf("YEAR: %i  DAY: %i  VALUE: %12.3f\n", i,j,q[i][j]);


//**************************  High Flow Conditions  *****************************

/*	double FH1, LFH1, UFH1;	// High flood pulse count 1
 *	double FH2;				// Variability in high flood pulse count 1
 *	double FH3, LFH3, UFH3;	// High flood pulse count 2
 *	double FH4, LFH4, UFH4;	// High flood pulse count 2
 *	double FH5, LFH5, UFH5;	// Flood frequency 1
 *	double FH6, LFH6, UFH6;	// Flood frequency 1
 *	double FH7, LFH7, UFH7;	// Flood frequency 1
 *	double FH8, LFH8, UFH8;	// Flood frequency 2
 *	double FH9, LFH9, UFH9;	// Flood frequency 2
 *	double FH10, LFH10, UFH10;		// Flood frequency 3
 *	double FH11, LFH11, UFH11;		// Flood frequency 4 */
	double p75;				// 75th percentile flow

	p75 = percentile(.75,ndv,data);
	for(i=0;i<nyrs;i++)
	{
		np[i] = 0.;				// Number of pulses per year
		hfdur[i] = 0.;			// Average duration of pulses for each year
		pdur = flag = 0;
		for(j=0;j<366;j++)
		{
			if(q[i][j] >= nearhuge) continue;
			if(q[i][j] > p75)
			{
				pdur++;		// Increment days in a pulse
				flag++;
				if(flag==1) np[i]++;	// Add a new pulse
			}
			else
			{
				flag = 0;
			}
		}
		if(np[i]>0.) hfdur[i] = pdur / np[i];	// Compute average duration per year
	}
	stats(nyrs,np,&qmean,&qmedian,&qstdv);

// FH[1]		High flood pulse count 1.  Compute the average number of flow events with flows above a
//			threshold equal to the 75th percentile value for the entire flow record.  FH[1] is the average
//			number of events.  (number of events)

	FH[1] = qmean;
	if(usemedian) FH[1] = qmedian;
	LFH[1] = percentile(lb,nyrs,np);
	UFH[1] = percentile(ub,nyrs,np);
	if(LFH[1] > FH[1]) LFH[1] = FH[1];
	if(UFH[1] < FH[1]) UFH[1] = FH[1];

// FH[2]		Variability in high pulse count 1.  Compute the standard deviation in the annual pulse counts for
//			FH[1].  FH[2] is 100 times the standard deviation divided by the mean pulse count (FH[1]).  (%)

	FH[2] = 999999;
	if(FH[1] != 0.) FH[2] = 100.*qstdv / FH[1];

	threshold = 3.*MA[2];
	for(i=0;i<nyrs;i++)
	{
		np[i] = 0.;			// Number of pulses per year
		flag = 0;
		for(j=0;j<366;j++)
		{
			if(q[i][j] >= nearhuge) continue;
			if(q[i][j] > threshold)
			{
				flag++;
				np[i]++;
			}
			else
			{
				flag = 0;
			}
		}
	}
	stats(nyrs,np,&qmean,&qmedian,&qstdv);

// FH[3]		High flood pulse count 2.  Compute the average number of days per year that the flow is above
//			a threshold equal to three times the median flow for the entire record.  FH[3] is the mean of the
//			annual number of days for all years.  (number of days)

	FH[3] = qmean;
	if(usemedian) FH[3] = qmedian;
	LFH[3] = percentile(lb,nyrs,np);
	UFH[3] = percentile(ub,nyrs,np);
	if(LFH[3] > FH[3]) LFH[3] = FH[3];
	if(UFH[3] < FH[3]) UFH[3] = FH[3];

	threshold = 7.*MA[2];
	for(i=0;i<nyrs;i++)
	{
		np[i] = 0.;		// Number of pulses per year
		flag = 0;
		for(j=0;j<366;j++)
		{
			if(q[i][j] >= nearhuge) continue;
			if(q[i][j] > threshold)
			{
				flag++;
				np[i]++;
			}
			else
			{
				flag = 0;
			}
		}
	}
	stats(nyrs,np,&qmean,&qmedian,&qstdv);

// FH[4]		High flood pulse count 2.  Compute the average number of days per year that the flow is above
//			a threshold equal to seven times the median flow for the entire record.  FH[4] is the mean of the
//			annual number of days for all years.  (number of days)

	FH[4] = qmean;
	if(usemedian) FH[4] = qmedian;
	LFH[4] = percentile(lb,nyrs,np);
	UFH[4] = percentile(ub,nyrs,np);
	if(LFH[4] > FH[4]) LFH[4] = FH[4];
	if(UFH[4] < FH[4]) UFH[4] = FH[4];

// Compute the number of events (pulses) per year above threshold

	threshold = MA[2];
	for(i=0;i<nyrs;i++)
	{
		np[i] = 0.;		// Number of pulses per year
		flag = 0;
		for(j=0;j<366;j++)
		{
			if(q[i][j] >= nearhuge) continue;
			if(q[i][j] > threshold)
			{
				flag++;
				if(flag==1) np[i]++;
			}
			else
			{
				flag = 0;
			}
		}
	}
	stats(nyrs,np,&qmean,&qmedian,&qstdv);

// FH[5]		Flood frequency 1.  Compute the average number of flow events with flows above a
//			threshold equal to the median flow value for the entire flow record.  FH[5] is the average
//			number of events.  (number of events)

	FH[5] = qmean;
	if(usemedian) FH[5] = qmedian;
	LFH[5] = percentile(lb,nyrs,np);
	UFH[5] = percentile(ub,nyrs,np);
	if(LFH[5] > FH[5]) LFH[5] = FH[5];
	if(UFH[5] < FH[5]) UFH[5] = FH[5];

// Compute the number of events (pulses) per year above threshold

	threshold = 3.*MA[2];
	for(i=0;i<nyrs;i++)
	{
		np[i] = 0.;		// Number of pulses per year
		flag = 0;
		for(j=0;j<366;j++)
		{
			if(q[i][j] >= nearhuge) continue;
			if(q[i][j] > threshold)
			{
				flag++;
				if(flag==1) np[i]++;
			}
			else
			{
				flag = 0;
			}
		}
	}
	stats(nyrs,np,&qmean,&qmedian,&qstdv);

// FH[6]		Flood frequency 1.  Compute the average number of flow events with flows above a
//			threshold equal to three times the median flow value for the entire flow record.  FH[6] is the
//			average number of events.  (number of events)

	FH[6] = qmean;
	if(usemedian) FH[6] = qmedian;
	LFH[6] = percentile(lb,nyrs,np);
	UFH[6] = percentile(ub,nyrs,np);
	if(LFH[6] > FH[6]) LFH[6] = FH[6];
	if(UFH[6] < FH[6]) UFH[6] = FH[6];

// Compute the number of events (pulses) per year above threshold

	threshold = 7.*MA[2];
	for(i=0;i<nyrs;i++)
	{
		np[i] = 0.;		// Number of pulses per year
		flag = 0;
		for(j=0;j<366;j++)
		{
			if(q[i][j] >= nearhuge) continue;
			if(q[i][j] > threshold)
			{
				flag++;
				if(flag==1) np[i]++;
			}
			else
			{
				flag = 0;
			}
		}
	}
	stats(nyrs,np,&qmean,&qmedian,&qstdv);

// FH[7]		Flood frequency 1.  Compute the average number of flow events with flows above a
//			threshold equal to seven times the median flow value for the entire flow record.  FH[6] is the
//			average number of events.  (number of events)

	FH[7] = qmean;
	if(usemedian) FH[7] = qmedian;
	LFH[7] = percentile(lb,nyrs,np);
	UFH[7] = percentile(ub,nyrs,np);
	if(LFH[7] > FH[7]) LFH[7] = FH[7];
	if(UFH[7] < FH[7]) UFH[7] = FH[7];

// Compute the number of events (pulses) per year above threshold

	order(0,data,ndv);		// Order data in descending order
	threshold = percentile(.25,ndv,data);
	for(i=0;i<nyrs;i++)
	{
		np[i] = 0.;		// Number of pulses per year
		flag = 0;
		for(j=0;j<366;j++)
		{
			if(q[i][j] >= nearhuge) continue;
			if(q[i][j] > threshold)
			{
				flag++;
				if(flag==1) np[i]++;
			}
			else
			{
				flag = 0;
			}
		}
	}
	stats(nyrs,np,&qmean,&qmedian,&qstdv);

// FH[8]		Flood frequency 2.  Compute the average number of flow events with flows above a
//			threshold equal to 25% exceedence value for the entire flow record.  FH[8] is the
//			average number of events.  (number of events)

	FH[8] = qmean;
	if(usemedian) FH[8] = qmedian;
	LFH[8] = percentile(lb,nyrs,np);
	UFH[8] = percentile(ub,nyrs,np);
	if(LFH[8] > FH[8]) LFH[8] = FH[8];
	if(UFH[8] < FH[8]) UFH[8] = FH[8];

// Compute the number of events (pulses) per year above threshold

	threshold = percentile(.75,ndv,data);
	for(i=0;i<nyrs;i++)
	{
		np[i] = 0.;		// Number of pulses per year
		flag = 0;
		for(j=0;j<366;j++)
		{
			if(q[i][j] >= nearhuge) continue;
			if(q[i][j] > threshold)
			{
				flag++;
				if(flag==1) np[i]++;
			}
			else
			{
				flag = 0;
			}
		}
	}
	stats(nyrs,np,&qmean,&qmedian,&qstdv);

// FH[9]		Flood frequency 2.  Compute the average number of flow events with flows above a
//			threshold equal to 75% exceedence value for the entire flow record.  FH[9] is the
//			average number of events.  (number of events)

	FH[9] = qmean;
	if(usemedian) FH[9] = qmedian;
	LFH[9] = percentile(lb,nyrs,np);
	UFH[9] = percentile(ub,nyrs,np);
	if(LFH[9] > FH[9]) LFH[9] = FH[9];
	if(UFH[9] < FH[9]) UFH[9] = FH[9];

// Compute the threshold as the median of the yearly minimum flows

	threshold = min_med;

// Compute the number of events (pulses) per year above threshold

	for(i=0;i<nyrs;i++)
	{
		np[i] = 0.;		// Number of pulses per year
		flag = 0;
		for(j=0;j<366;j++)
		{
			if(q[i][j] >= nearhuge) continue;
			if(q[i][j] > threshold)
			{
				flag++;
				if(flag==1) np[i]++;
			}
			else
			{
				flag = 0;
			}
		}
	}
	stats(nyrs,np,&qmean,&qmedian,&qstdv);

// FH[10]		Flood frequency 3.  Compute the average number of flow events with flows above a
//			threshold equal to median of the annual minima for the entire flow record.  FH[10] is the
//			average number of events.  (number of events)

	FH[10] = qmean;
	if(usemedian) FH[10] = qmedian;
	LFH[10] = percentile(lb,nyrs,np);
	UFH[10] = percentile(ub,nyrs,np);
	if(LFH[10] > FH[10]) LFH[10] = FH[10];
	if(UFH[10] < FH[10]) UFH[10] = FH[10];

// Compute the number of events having a magnitude > that of a 1.67 year flood
// (percentile of 60%)

// Compute the 1.67 year flood threshold

// 1.67-year bankfull threshold (Poff, 1996) - For indices FH[11], DH22, DH23, DH24, TA3, and TH3
// compute the log10 of the peak annual flows. Compute the log10 of the daily flows for the peak
// annual flow days. Calculate the coefficients for a linear regression equation for logs of peak
// annual flow versus logs of average daily flow for peak days. Using the log peak flow for the
// 1.67 year recurrence interval (60th percentile) as input to the regression equation, predict
// the log10 of the average daily flow. The threshold is 10 to the log10 (average daily flow)
// power (cubic feet/second).

	double l24[150];			// Log10 of average daily flow for the peak flow day
	double lpeak[150];		// Log10 of annual peak hourly flows
	double xmean;			// Mean of the logs of peak flows
	double ymean;			// Mean of the logs of average daily flows
	double lq167;			// Log10 of 1.67 year threshold
	double lq5;				// Log10 of 5 year threshold
	double numerator, denominator;
	double a, b;

// Compute the logs of 24hr flows

	if(dopeak)
	{
		total = 0.;
		for(i=0;i<npyrs;i++)
		{
			l24[i] = (double)log10((double)f24[i]);
			total = total + l24[i];
		}
		ymean = total / npyrs;

// Compute the logs of peak annual flows

		total = 0.;
		for(i=0;i<npyrs;i++)
		{
			lpeak[i] = (double)log10((double)peak[i]);
			total = total + lpeak[i];
		}
		xmean = total / npyrs;

// Compute the regression coefficients for log(meana) vs log(peak)

		numerator = denominator = 0.;
		for(i=0;i<npyrs;i++)
		{
			numerator = numerator + (lpeak[i]-xmean)*(l24[i]-ymean);
			denominator = denominator + (lpeak[i]-xmean)*(lpeak[i]-xmean);
		}
		b = numerator / denominator;
		a = ymean - b*xmean;

// Find the 60th percentile of the logs of the peak flows

		order(1,lpeak,npyrs);		// Order data in ascending order
		temp = percentile(.60,npyrs,lpeak);
		lq167 = a + b*temp;
		threshold = (double)pow(10.,(double)lq167);

		for(i=0;i<nyrs;i++)
		{
			np[i] = 0.;		// Number of pulses per year
			flag = 0;
			for(j=0;j<366;j++)
			{
				if(q[i][j] >= nearhuge) continue;
				if(q[i][j] > threshold)
				{
					flag++;
					if(flag==1) np[i]++;
				}
				else
				{
					flag = 0;
				}
			}
		}
		stats(nyrs,np,&qmean,&qmedian,&qstdv);

// FH[11]		Flood frequency 4.  Compute the average number of flow events with flows above a
//			threshold equal to flow corresponding to a 1.67 year recurrence interval.  FH[11] is the
//			average number of events.  (number of events)

		FH[11] = qmean;
		if(usemedian) FH[11] = qmedian;
		LFH[11] = percentile(lb,nyrs,np);
		UFH[11] = percentile(ub,nyrs,np);
//		if(LFH[11] > FH[11]) LFH[11] = FH[11];
//		if(UFH[11] < FH[11]) UFH[11] = FH[11];
	}

//***************************  Duration of flow events  *************************

/*	double DL1, LDL1, UDL1;		// Annual minimum of 1 day means of daily discharge
 *	double DL2, LDL2, UDL2;		// Annual minimum of 3 day means of daily discharge
 *	double DL3, LDL3, UDL3;		// Annual minimum of 7 day means of daily discharge
 *	double DL4, LDL4, UDL4;		// Annual minimum of 30 day means of daily discharge
 *	double DL5, LDL5, UDL5;		// Annual minimum of 90 day means of daily discharge
 *	double DL6, DL7, DL8, DL9, DL10;	// Variability in annual minima of 1,2,7,30,90 day means of daily discharge
 *	double DL11, LDL11, UDL11;		// Means of 1 day minimum of daily discharge
 *	double DL12, LDL12, UDL12;		// Means of 7 day minimum of daily discharge
 *	double DL13, LDL13, UDL13;		// Means of 30 day minimum of daily discharge
 *	double DL14, DL15;				// Low exceedence flows (75% and 90%)
 *	double DL16, LDL16, UDL16;		// Low flow pulse duration
 *	double DL17;						// Variability in low flow pulse duration
 *	double DL18, LDL18, UDL18;		// Number of zero-flow days
 *	double DL19;						// Variability in number of zero-flow days
 *	double DL20;						// Percent of zero-flow months
 */
/* 	double DH1, LDH1, UDH1;		// Annual maximum of 1 day means of daily discharge
 * 	double DH2, LDH2, UDH2;		// Annual maximum of 3 day means of daily discharge
 * 	double DH3, LDH3, UDH3;		// Annual maximum of 7 day means of daily discharge
 * 	double DH4, LDH4, UDH4;		// Annual maximum of 30 day means of daily discharge
 * 	double DH5, LDH5, UDH5;		// Annual maximum of 90 day means of daily discharge
 * 	double DH6, DH7, DH8, DH9, DH10;	// Variability in annual maxima of 1,2,7,30,90 day means of daily discharge
 * 	double DH11, LDH11, UDH11;		// Means of 1 day maximum of daily discharge
 * 	double DH12, LDH12, UDH12;		// Means of 7 day maximum of daily discharge
 * 	double DH13, LDH13, UDH13;		// Means of 30 day maximum of daily discharge
 * 	double DH14;						// Flood duration 1
 * 	double DH15, LDH15, UDH15;		// High flow pulse duration
 * 	double DH16;						// Variability in high flow pulse duration
 * 	double DH17, LDH17, UDH17;		// High flow duration 1
 * 	double DH18, LDH18, UDH18;		// High flow duration 1
 * 	double DH19, LDH19, UDH19;		// High flow duration 1
 * 	double DH20, LDH20, UDH20;		// High flow duration 2
 * 	double DH21, LDH21, UDH21;		// High flow duration 2
 * 	double DH22, LDH22, UDH22;				// Flood interval
 * 	double DH23, LDH23, UDH23;				// Flood duration 2
 * 	double DH24, LDH24, UDH24;				// Flood free days */

//***************************  Low flow conditions  *****************************

// Compute annual minima of 1, 3, 7, 30, 90 day means of daily discharge (IHA)

	for(i=0;i<nyrs;i++)		// 1 day
	{
		minqm = 10000000.;
		for(j=0;j<366;j++)
		{
			if(q[i][j] < nearhuge)
			{
				if(q[i][j] < minqm) minqm = q[i][j];
			}
		}
		mdata[i] = minqm;
	}
	stats(nyrs,mdata,&qmean,&qmedian,&qstdv);

// DL1 		Annual minimum of 1-day moving average flows.  Compute the minimum of a 1-day moving
//			average flow for each year.  DL1 is the mean of these values.  (cfs)

	DL[1] = qmean;
	if(usemedian) DL[1] = qmedian;
	LDL[1] = percentile(lb,nyrs,mdata);
	UDL[1] = percentile(ub,nyrs,mdata);
	if(LDL[1] > DL[1]) LDL[1] = DL[1];
	if(UDL[1] < DL[1]) UDL[1] = DL[1];

// DL[6]		Variability of annual minimum of 1-day moving average flows.  Compute the standard deviation
//			for the minimum 1-day moving averages.  DL[6] is 100 times the standard deviation divided by
//			the mean.  (%)
//			Limits are not computed.

	DL[6] = 999999;
	if(qmean != 0.) DL[6] = 100.*qstdv/qmean;

	for(i=0;i<nyrs;i++)		// 3 day
	{
		minqm = 10000000.;
		for(j=0;j<364;j++)
		{
			total = 0.;
			nmv = 0;
			for(k=j;k<j+3;k++)
			{
				if(q[i][k] < nearhuge)
				{
					total = total + q[i][k];
					nmv++;
				}
			}
			mdata[i] = total / nmv;		// Store 3 day average
			if(mdata[i] < minqm) minqm = mdata[i];	// Compute minimum of 3 day averages
		}
		mdata[i] = minqm;		// Store minimum of 3 day averages
	}
	stats(nyrs,mdata,&qmean,&qmedian,&qstdv);

// DL[2] 		Annual minimum of 3-day moving average flows.  Compute the minimum of a 3-day moving
//			average flow for each year.  DL[2] is the mean of these values.  (cfs)

	DL[2] = qmean;
	if(usemedian) DL[2] = qmedian;
	LDL[2] = percentile(lb,nyrs,mdata);
	UDL[2] = percentile(ub,nyrs,mdata);
	if(LDL[2] > DL[2]) LDL[2] = DL[2];
	if(UDL[2] < DL[2]) UDL[2] = DL[2];

// DL[7]		Variability of annual minimum of 3-day moving average flows.  Compute the standard deviation
//			for the minimum 3-day moving averages.  DL[7] is 100 times the standard deviation divided by
//			the mean.  (%)
//			Limits are not computed.

	DL[7] = 999999;
	if(qmean != 0.) DL[7] = 100.*qstdv/qmean;

	for(i=0;i<nyrs;i++)		// 7day
	{
		minqm = 10000000.;
		for(j=0;j<360;j++)
		{
			total = 0.;
			nmv = 0;
			for(k=j;k<j+7;k++)
			{
				if(q[i][k] < nearhuge)
				{
					total = total + q[i][k];
					nmv++;
				}
			}
			mdata[i] = total / nmv;		// Compute 7 day averages
			if(mdata[i] < minqm) minqm = mdata[i];	// Compute minimum of 7 day averages
		}
		mdata[i] = minqm;	// Store the yearly minimum of 7 day averages
	}
	stats(nyrs,mdata,&qmean,&qmedian,&qstdv);

// DL[3] 		Annual minimum of 7-day moving average flows.  Compute the minimum of a 7-day moving
//			average flow for each year.  DL[3] is the mean of these values.  (cfs)

	DL[3] = qmean;
	if(usemedian) DL[3] = qmedian;
	LDL[3] = percentile(lb,nyrs,mdata);
	UDL[3] = percentile(ub,nyrs,mdata);
	if(LDL[3] > DL[3]) LDL[3] = DL[3];
	if(UDL[3] < DL[3]) UDL[3] = DL[3];

// DL[8]		Variability of annual minimum of 7-day moving average flows.  Compute the standard deviation
//			for the minimum 7-day moving averages.  DL[8] is 100 times the standard deviation divided by
//			the mean.  (%)
//			Limits are not computed.

	DL[8] = 999999;
	if(qmean != 0.) DL[8] = 100.*qstdv/qmean;

	for(i=0;i<nyrs;i++)		// 30 day
	{
		minqm = 10000000.;
		for(j=0;j<337;j++)
		{
			total = 0.;
			nmv = 0;
			for(k=j;k<j+30;k++)
			{
				if(q[i][k] < nearhuge)
				{
					total = total + q[i][k];
					nmv++;
				}
			}
			mdata[i] = total / nmv;		// Compute 30 day average
			if(mdata[i] < minqm) minqm = mdata[i];	// Compute the minimum of the 30 day averages
		}
		mdata[i] = minqm;	// Store the minimum 30 day average for the year
	}
	stats(nyrs,mdata,&qmean,&qmedian,&qstdv);

// DL[4] 		Annual minimum of 30-day moving average flows.  Compute the minimum of a 30-day moving
//			average flow for each year.  DL[4] is the mean of these values.  (cfs)

	DL[4] = qmean;
	if(usemedian) DL[4] = qmedian;
	LDL[4] = percentile(lb,nyrs,mdata);
	UDL[4] = percentile(ub,nyrs,mdata);
	if(LDL[4] > DL[4]) LDL[4] = DL[4];
	if(UDL[4] < DL[4]) UDL[4] = DL[4];

// DL[9]		Variability of annual minimum of 30-day moving average flows.  Compute the standard
//			deviation for the minimum 30-day moving averages.  DL[9] is 100 times the standard deviation
//			divided by the mean.  (%)
//			Limits are not computed.

	DL[9] = 999999;
	if(qmean != 0.) DL[9] = 100.*qstdv/qmean;

	for(i=0;i<nyrs;i++)		// 90 day
	{
		minqm = 10000000.;
		for(j=0;j<277;j++)
		{
			total = 0.;
			nmv = 0;
			for(k=j;k<j+90;k++)
			{
				if(q[i][k] < nearhuge)
				{
					total = total + q[i][k];
					nmv++;
				}
			}
			mdata[i] = total / nmv;		// Compute the 90 day average
			if(mdata[i] < minqm) minqm = mdata[i];	// Compute the minimum of the 90 day averages
		}
		mdata[i] = minqm;	// Store minimum 90 day average
	}
	stats(nyrs,mdata,&qmean,&qmedian,&qstdv);

// DL[5] 		Annual minimum of 90-day moving average flows.  Compute the minimum of a 90-day moving
//			average flow for each year.  DL[5] is the mean of these values.  (cfs)

	DL[5] = qmean;
	if(usemedian) DL[5] = qmedian;
	LDL[5] = percentile(lb,nyrs,mdata);
	UDL[5] = percentile(ub,nyrs,mdata);
	if(LDL[5] > DL[5]) LDL[5] = DL[5];
	if(UDL[5] < DL[5]) UDL[5] = DL[5];

// DL[10]		Variability of annual minimum of 90-day moving average flows.  Compute the standard
//			deviation for the minimum 90-day moving averages.  DL[10] is 100 times the standard deviation
//			divided by the mean.  (%)
//			Limits are not computed.

	DL[10] = 999999;
	if(qmean != 0.) DL[10] = 100.*qstdv/qmean;

// Compute Means of 1,7,30 day minima of daily discharge

	for(i=0;i<nyrs;i++)		// 1 day minima
	{
		mdata[i] = minaq[i];
	}
	stats(nyrs,mdata,&qmean,&qmedian,&qstdv);

// DL[11]		Annual minimum of 1-day moving average flows divided by the median for the entire record.
//			Compute the minimum of a 1-day moving average flow for each year.  DL[11] is the mean of 			these values divided by the median for the entire record.  (dimensionless)

	DL[11] = 999999;
	if(MA[2] != 0.) DL[11] = qmean / MA[2];
	temp = percentile(lb,nyrs,mdata);
	LDL[11] = 999999;
	if(MA[2] != 0.) LDL[11] = temp / MA[2];
	temp = percentile(ub,nyrs,mdata);
	UDL[11] = 999999;
	if(MA[2] != 0.) UDL[11] = temp / MA[2];
	if(LDL[11] > DL[11]) LDL[11] = DL[11];
	if(UDL[11] < DL[11]) UDL[11] = DL[11];

	for(i=0;i<nyrs;i++)		// 7 day minima
	{
		minqm = 10000000.;
		for(j=0;j<360;j++)
		{
			total = 0.;
			nmv = 0;
			for(k=j;k<j+7;k++)
			{
				if(q[i][k] < nearhuge)
				{
					total = total + q[i][k];
					nmv++;
				}
			}
			mdata[i] = total / nmv;		// Compute the 7 day averages of daily flows
			if(mdata[i] < minqm) minqm = mdata[i];	// Compute the minimum of the averages
		}
		mdata[i] = minqm;	// Store the minimum
	}
	stats(nyrs,mdata,&qmean,&qmedian,&qstdv);

// DL[12]		Annual minimum of 7-day moving average flows divided by the median for the entire record.
//			Compute the minimum of a 7-day moving average flow for each year.  DL[12] is the mean of 			these values divided by the median for the entire record.  (dimensionless)

	DL[12] = 999999;
	if(MA[2] != 0.) DL[12] = qmean / MA[2];
	temp = percentile(lb,nyrs,mdata);
	LDL[12] = 999999;
	if(MA[2] != 0.) LDL[12] = temp / MA[2];
	temp = percentile(ub,nyrs,mdata);
	UDL[12] = 999999;
	if(MA[2] != 0.) UDL[12] = temp / MA[2];
	if(LDL[12] > DL[12]) LDL[12] = DL[12];
	if(UDL[12] < DL[12]) UDL[12] = DL[12];

	for(i=0;i<nyrs;i++)		// 30 day minima
	{
		minqm = 10000000.;
		for(j=0;j<337;j++)
		{
			total = 0.;
			nmv = 0;
			for(k=j;k<j+30;k++)
			{
				if(q[i][k] < nearhuge)
				{
					total = total + q[i][k];
					nmv++;
				}
			}
			mdata[i] = total / nmv;	// Compute the 30 day average flow
			if(mdata[i] < minqm) minqm = mdata[i]; // Compute the minimum of the averages
		}
		mdata[i] = minqm;	// Store the minimum
	}
	stats(nyrs,mdata,&qmean,&qmedian,&qstdv);

// DL[13]		Annual minimum of 30-day moving average flows divided by the median for the entire record.
//			Compute the minimum of a 30-day moving average flow for each year.  DL[13] is the mean of 			these values divided by the median for the entire record.  (dimensionless)

	DL[13] = 999999;
	if(MA[2] != 0.) DL[13] = qmean / MA[2];
	temp = percentile(lb,nyrs,mdata);
	LDL[13] = 999999;
	if(MA[2] != 0.) LDL[13] = temp / MA[2];
	temp = percentile(ub,nyrs,mdata);
	UDL[13] = 999999;
	if(MA[2] != 0.) UDL[13] = temp / MA[2];
	if(LDL[13] > DL[13]) LDL[13] = DL[13];
	if(UDL[13] < DL[13]) UDL[13] = DL[13];

// Compute low exceedence flows (75% and 90%)

	order(0,data,ndv);		// Order data in descending order

// DL[14]		Low exceedence flows.  Compute the 75% exceedence value for the entire flow record.
//			DL[14] is the exceedence value divided by the median for the entire record.  (dimensionless)
//			Limits are not computed.

	temp = percentile(.75,ndv,data);
	DL[14] = 999999;
	if(MA[2] != 0.) DL[14] = temp / MA[2];

// DL[15]		Low exceedence flows.  Compute the 90% exceedence value for the entire flow record.
//			DL[14] is the exceedence value divided by the median for the entire record.  (dimensionless)
//			Limits are not computed.

	temp = percentile(.90,ndv,data);
	DL[15] = 999999;
	if(MA[2] != 0.) DL[15] = temp / MA[2];

// Compute low flow pulse duration (IHA) - DL[16]

	nmv = 0;
	for(i=0;i<nyrs;i++)
	{
		if(lfdur[i] > 0.)
		{
			mdata[nmv] = lfdur[i];
			nmv++;
		}
	}
	qmedian = qmean = qstdv = 0.;
	if(nmv > 0) stats(nmv,mdata,&qmean,&qmedian,&qstdv);

// DL[16]		Low flow pulse duration.  Compute the average pulse duration for each year for flow events
//			below a threshold equal to the 25th percentile value for the entire flow record.  DL[16] is the
//			median of the yearly average durations.  (number of days)

	DL[16] = qmedian;
	LDL[16] = percentile(lb,nmv,mdata);
	UDL[16] = percentile(ub,nmv,mdata);
	if(LDL[16] > DL[16]) LDL[16] = DL[16];
	if(UDL[16] < DL[16]) UDL[16] = DL[16];

// Compute variability in low flow pulse duration (IHA) - DL[17]

// DL[17]		Variability in low pulse duration.  Compute the standard deviation for the yearly average low
//			pulse durations.  DL[17] is 100 times the standard deviation divided by the mean of the yearly
//			average low pulse durations.  (%)
//			Limits are not computed.

	DL[17] = 999999;
	if(qmean != 0.) DL[17] = (qstdv/qmean)*100.;

// Compute number of zero-flow days (IHA) - DL[18]

	for(i=0;i<nyrs;i++)
	{
		mdata[i] = 0.;
		for(j=0;j<366;j++)
		{
			if(q[i][j]==0.)
			{
				mdata[i]++;
			}
		}
	}
	stats(nyrs,mdata,&qmean,&qmedian,&qstdv);

// DL[18]		Number of zero-flow days.  Count the number of zero-flow days for the entire flow record.
//			DL[18] is the mean annual number of zero flow days.	 (number of days)

	DL[18] = qmean;
	if(usemedian) DL[18] = qmedian;
	LDL[18] = percentile(lb,nyrs,mdata);
	UDL[18] = percentile(ub,nyrs,mdata);
	if(LDL[18] > DL[18]) LDL[18] = DL[18];
	if(UDL[18] < DL[18]) UDL[18] = DL[18];

// Compute variability in number of zero-flow days (IHA) - DL[19]

// DL[19]		Variability in the number of zero-flow days.  Compute the standard deviation for the annual
//			number of zero-flow days.  DL[19] is 100 times the standard deviation divided by the mean
//			annual number of zero-flow days.  (%)
//			Limits are not computed.

	DL[19] = 999999;
	if(qmean != 0.) DL[19] = 100.*qstdv/qmean;

// Compute percent of zero-flow months

// DL[20]		Number of zero-flow months.  While computing the mean monthly flow values, count the
//			number of months in which there was no flow over the entire flow record.  (number of months)

	DL[20] = (double)zcount*100. / (12.*(double)nyrs);	// zcount is computed with monthly statistics


//***************************  High flow conditions  *****************************

// Compute annual maxima of 1, 3, 7, 30, 90 day means of daily discharge (IHA)

	for(i=0;i<nyrs;i++)		// 1 day
	{
		maxqm = -10000000.;
		for(j=0;j<366;j++)
		{
			if(q[i][j] < nearhuge)
			{
				if(q[i][j] > maxqm) maxqm = q[i][j];
			}
		}
		mdata[i] = maxqm;
	}
	stats(nyrs,mdata,&qmean,&qmedian,&qstdv);

// DH[1] 		Annual maximum of 1-day moving average flows.  Compute the maximum of a 1-day moving
//			average flow for each year.  DH[1] is the mean of these values.  (cfs)

	DH[1] = qmean;
	if(usemedian) DH[1] = qmedian;
	LDH[1] = percentile(lb,nyrs,mdata);
	UDH[1] = percentile(ub,nyrs,mdata);
	if(LDH[1] > DH[1]) LDH[1] = DH[1];
	if(UDH[1] < DH[1]) UDH[1] = DH[1];

// DH[6]		Variability of annual maximum of 1-day moving average flows.  Compute the standard deviation
//			for the maximum 1-day moving averages.  DH[6] is 100 times the standard deviation divided by
//			the mean.  (%)
//			Limits are not computed.

	DH[6] = 999999;
	if(qmean != 0.) DH[6] = 100.*qstdv/qmean;

	for(i=0;i<nyrs;i++)		// 3 day
	{
		maxqm = -10000000.;
		for(j=0;j<364;j++)
		{
			total = 0.;
			nmv = 0;
			for(k=j;k<j+3;k++)
			{
				if(q[i][k] < nearhuge)
				{
					total = total + q[i][k];
					nmv++;
				}
			}
			mdata[i] = total / nmv;		// Compute the 3 day average
			if(mdata[i] > maxqm) maxqm = mdata[i];	// Compute the maximum
		}
		mdata[i] = maxqm;	// Store the maximum
	}
	stats(nyrs,mdata,&qmean,&qmedian,&qstdv);

// DH[2] 		Annual minimum of 3-day moving average flows.  Compute the minimum of a 3-day moving
//			average flow for each year.  DH[2] is the mean of these values.  (cfs)

	DH[2] = qmean;
	if(usemedian) DH[2] = qmedian;
	LDH[2] = percentile(lb,nyrs,mdata);
	UDH[2] = percentile(ub,nyrs,mdata);
	if(LDH[2] > DH[2]) LDH[2] = DH[2];
	if(UDH[2] < DH[2]) UDH[2] = DH[2];

// DH[7]		Variability of annual maximum of 3-day moving average flows.  Compute the standard deviation
//			for the maximum 3-day moving averages.  DH[7] is 100 times the standard deviation divided by
//			the mean.  (%)
//			Limits are not computed.

	DH[7] = 999999;
	if(qmean != 0.) DH[7] = 100.*qstdv/qmean;

	for(i=0;i<nyrs;i++)		// 7day
	{
		maxqm = -10000000.;
		for(j=0;j<360;j++)
		{
			total = 0.;
			nmv = 0;
			for(k=j;k<j+7;k++)
			{
				if(q[i][k] < nearhuge)
				{
					total = total + q[i][k];
					nmv++;
				}
			}
			mdata[i] = total / nmv;		// Compute the 7 day average
			if(mdata[i] > maxqm) maxqm = mdata[i];	// Compute the maximum
		}
		mdata[i] = maxqm;	// Store the maximum
	}
	stats(nyrs,mdata,&qmean,&qmedian,&qstdv);

// DH[3] 		Annual maximum of 7-day moving average flows.  Compute the maximum of a 7-day moving
//			average flow for each year.  DH[3] is the mean of these values.  (cfs)

	DH[3] = qmean;
	if(usemedian) DH[3] = qmedian;
	LDH[3] = percentile(lb,nyrs,mdata);
	UDH[3] = percentile(ub,nyrs,mdata);
	if(LDH[3] > DH[3]) LDH[3] = DH[3];
	if(UDH[3] < DH[3]) UDH[3] = DH[3];

// DH[8]		Variability of annual maximum of 7-day moving average flows.  Compute the standard deviation
//			for the maximum 7-day moving averages.  DH[8] is 100 times the standard deviation divided by
//			the mean.  (%)
//			Limits are not computed.

	DH[8] = 999999;
	if(qmean != 0.) DH[8] = 100.*qstdv/qmean;

	for(i=0;i<nyrs;i++)		// 30 day
	{
		maxqm = -10000000.;
		for(j=0;j<337;j++)
		{
			total = 0.;
			nmv = 0;
			for(k=j;k<j+30;k++)
			{
				if(q[i][k] < nearhuge)
				{
					total = total + q[i][k];
					nmv++;
				}
			}
			mdata[i] = total / nmv;		// Compute the 30 day average
			if(mdata[i] > maxqm) maxqm = mdata[i];	// Compute the maximum
		}
		mdata[i] = maxqm;	// Store the maximum
	}
	stats(nyrs,mdata,&qmean,&qmedian,&qstdv);

// DH[4] 		Annual maximum of 30-day moving average flows.  Compute the maximum of a 30-day moving
//			average flow for each year.  DH[4] is the mean of these values.  (cfs)

	DH[4] = qmean;
	if(usemedian) DH[4] = qmedian;
	LDH[4] = percentile(lb,nyrs,mdata);
	UDH[4] = percentile(ub,nyrs,mdata);
	if(LDH[4] > DH[4]) LDH[4] = DH[4];
	if(UDH[4] < DH[4]) UDH[4] = DH[4];

// DH[9]		Variability of annual maximum of 30-day moving average flows.  Compute the standard
//			deviation for the maximum 30-day moving averages.  DH[9] is 100 times the standard deviation
//			divided by the mean.  (%)
//			Limits are not computed.

	DH[9] = 999999;
	if(qmean != 0.) DH[9] = 100.*qstdv/qmean;

	for(i=0;i<nyrs;i++)		// 90 day
	{
		maxqm = -10000000.;
		for(j=0;j<277;j++)
		{
			total = 0.;
			nmv = 0;
			for(k=j;k<j+90;k++)
			{
				if(q[i][k] < nearhuge)
				{
					total = total + q[i][k];
					nmv++;
				}
			}
			mdata[i] = total / nmv;		// Compute the 90 day average
			if(mdata[i] > maxqm) maxqm = mdata[i];	// Compute the maximum
		}
		mdata[i] = maxqm;	// Store the maximum
	}
	stats(nyrs,mdata,&qmean,&qmedian,&qstdv);

// DH[5] 		Annual maximum of 90-day moving average flows.  Compute the maximum of a 90-day moving
//			average flow for each year.  DH[5] is the mean of these values.  (cfs)

	DH[5] = qmean;
	if(usemedian) DH[5] = qmedian;
	LDH[5] = percentile(lb,nyrs,mdata);
	UDH[5] = percentile(ub,nyrs,mdata);
	if(LDH[5] > DH[5]) LDH[5] = DH[5];
	if(UDH[5] < DH[5]) UDH[5] = DH[5];

// DH[10]		Variability of annual maximum of 90-day moving average flows.  Compute the standard
//			deviation for the maximum 90-day moving averages.  DH[10] is 100 times the standard deviation
//			divided by the mean.  (%)
//			Limits are not computed.

	DH[10] = 999999;
	if(qmean != 0.) DH[10] = 100.*qstdv/qmean;

// Compute Means of 1,7,30 day maxima of daily discharge

	for(i=0;i<nyrs;i++)		// 1 day maxima
	{
		mdata[i] = maxaq[i];
	}
	stats(nyrs,mdata,&qmean,&qmedian,&qstdv);

// DH[11]		Annual maximum of 1-day moving average flows divided by the median for the entire record.
//			Compute the maximum of a 1-day moving average flow for each year.  DL[11] is the mean of 			these values divided by the median for the entire record.  (dimensionless)

	DH[11] = 999999;
	if(MA[2] != 0.) DH[11] = qmean / MA[2];
	temp = percentile(lb,nyrs,mdata);

	if(MA[2] != 0.) LDH[11] = temp / MA[2];
	temp = percentile(ub,nyrs,mdata);

	if(MA[2] != 0.) UDH[11] = temp / MA[2];
	if(LDH[11] > DH[11]) LDH[11] = DH[11];
	if(UDH[11] < DH[11]) UDH[11] = DH[11];

	for(i=0;i<nyrs;i++)		// 7 day maxima
	{
		maxqm = -10000000.;
		for(j=0;j<360;j++)
		{
			total = 0.;
			nmv = 0;
			for(k=j;k<j+7;k++)
			{
				if(q[i][k] < nearhuge)
				{
					total = total + q[i][k];
					nmv++;
				}
			}
			mdata[i] = total / nmv;		// Compute the 7 day average
			if(mdata[i] > maxqm) maxqm = mdata[i];	// Compute the maximum
		}
		mdata[i] = maxqm;	// Store the maximum
	}
	stats(nyrs,mdata,&qmean,&qmedian,&qstdv);

// DH[12]		Annual maximum of 7-day moving average flows divided by the median for the entire record.
//			Compute the maximum of a 1-day moving average flow for each year.  DL[12] is the mean of 			these values divided by the median for the entire record.  (dimensionless)

	DH[12] = 999999;
	if(MA[2] != 0.) DH[12] = qmean / MA[2];
	temp = percentile(lb,nyrs,mdata);
	LDH[12] = 999999;
	if(MA[2] != 0.) LDH[12] = temp / MA[2];
	temp = percentile(ub,nyrs,mdata);
	UDH[12] = 999999;
	if(MA[2] != 0.) UDH[12] = temp / MA[2];
	if(LDH[12] > DH[12]) LDH[12] = DH[12];
	if(UDH[12] < DH[12]) UDH[12] = DH[12];

	for(i=0;i<nyrs;i++)		// 30 day maxima
	{
		maxqm = -10000000.;
		for(j=0;j<337;j++)
		{
			total = 0.;
			nmv = 0;
			for(k=j;k<j+30;k++)
			{
				if(q[i][k] < nearhuge)
				{
					total = total + q[i][k];
					nmv++;
				}
			}
			mdata[i] = total / nmv;		// Compute the 30 day average
			if(mdata[i] > maxqm) maxqm = mdata[i];	// Compute the maximum
		}
		mdata[i] = maxqm;	// Store the maximum
	}
	stats(nyrs,mdata,&qmean,&qmedian,&qstdv);

// DH[13]		Annual maximum of 30-day moving average flows divided by the median for the entire record.
//			Compute the maximum of a 1-day moving average flow for each year.  DL[13] is the mean of 			these values divided by the median for the entire record.  (dimensionless)

	DH[13] = 999999;
	if(MA[2] != 0.) DH[13] = qmean / MA[2];
	temp = percentile(lb,nyrs,mdata);
	LDH[13] = 999999;
	if(MA[2] != 0.) LDH[13] = temp / MA[2];
	temp = percentile(ub,nyrs,mdata);
	UDH[13] = 999999;
	if(MA[2] != 0.) UDH[13] = temp / MA[2];
	if(LDH[13] > DH[13]) LDH[13] = DH[13];
	if(UDH[13] < DH[13]) UDH[13] = DH[13];

// Compute flood duration 1

	nmv = 0;
	for(i=0;i<12;i++)
	{
		for(j=0;j<nyrs;j++)
		{
			mdata[nmv] = meanm[i][j];
			nmv++;
		}
	}
	stats(nmv,mdata,&qmean,&qmedian,&qstdv);
	order(1,mdata,nmv);		// Order in ascending order for percentile

// DH[14]		Flood duration 1.  Compute the mean of the mean monthly flow values.  Find the 95th
//			percentile for the mean monthly flows.  DH[14] is the 95th percentile value divided by the
//			mean of the monthly means.  (dimensionless)
//			Limits are not computed.

	temp = percentile(.95,nmv,mdata);
	DH[14] = 999999;
	if(qmean != 0.) DH[14] = temp / qmean;

// Compute high flow pulse duration (IHA)

	nmv = 0;
	for(i=0;i<nyrs;i++)
	{
		if(hfdur[i] > 0.)		// Exclude years with no pulses
		{
			mdata[nmv] = hfdur[i];
			nmv++;
		}
	}
	stats(nmv,mdata,&qmean,&qmedian,&qstdv);

// DH[15]		High flow pulse duration.  Compute the average duration for flow events with flows above a
//			threshold equal to the 75th percentile value for each year in the flow record.  DH[15] is the median
//			of the yearly average durations.  (days)

	DH[15] = qmedian;
	LDH[15] = percentile(lb,nmv,mdata);
	UDH[15] = percentile(ub,nmv,mdata);
	if(LDH[15] > DH[15]) LDH[15] = DH[15];
	if(UDH[15] < DH[15]) UDH[15] = DH[15];

// Compute variability in high flow pulse duration (IHA)

// DH[16]		Variability in high flow pulse duration.  Compute the standard deviation for the yearly average
//			high pulse durations.  DH[16] is 100 times the standard deviation divided by the mean of the
//			yearly average high pulse durations.  (%)
//			Limits are not computed.

	DH[16] = 999999;
	if(qmean != 0.) DH[16] = (qstdv/qmean)*100.;

// Compute high flow duration 1

	threshold = MA[2];
	for(i=0;i<nyrs;i++)
	{
		np[i] = 0.;			// Number of pulses
		hfdur[i] = 0.;		// Average duration of pulses for each year
		pdur = flag = 0;
		for(j=0;j<366;j++)
		{
			if(q[i][j] >= nearhuge) continue;
			if(q[i][j] > threshold)
			{
				pdur++;		// Compile number of days in all pulses
				flag++;
				if(flag==1) np[i]++;	// Add a pulse
			}
			else
			{
				flag = 0;
			}
		}
		if(np[i] > 0.) hfdur[i] = pdur / np[i];	// Compute average duration
	}
	nmv = 0;
	for(i=0;i<nyrs;i++)
	{
		if(hfdur[i] > 0.)	// Exclude years with no pulses
		{
			mdata[nmv] = hfdur[i];
			nmv++;
		}
	}
	stats(nmv,mdata,&qmean,&qmedian,&qstdv);

// DH[17]		High flow duration 1.  Compute the average duration of flow events with flows above a
//			threshold equal to the median flow value for the entire flow record.  DH[17] is the average
//			duration of the events.  (days)

	DH[17] = qmean;
	if(usemedian) DH[17] = qmedian;
	LDH[17] = percentile(lb,nmv,mdata);
	UDH[17] = percentile(ub,nmv,mdata);
	if(LDH[17] > DH[17]) LDH[17] = DH[17];
	if(UDH[17] < DH[17]) UDH[17] = DH[17];

	threshold = 3.*MA[2];
	for(i=0;i<nyrs;i++)
	{
		np[i] = 0.;			// Number of pulses
		hfdur[i] = 0.;		// Average duration of pulses for each year
		pdur = flag = 0;
		for(j=0;j<366;j++)
		{
			if(q[i][j] >= nearhuge) continue;
			if(q[i][j] > threshold)
			{
				pdur++;		// Compile number of days in all pulses
				flag++;
				if(flag==1) np[i]++;	// Add a pulse
			}
			else
			{
				flag = 0;
			}
		}
		if(np[i]>0.) hfdur[i] = pdur / np[i];	// Compute average duration
	}
	nmv = 0;
	for(i=0;i<nyrs;i++)
	{
		if(hfdur[i] > 0.)
		{
			mdata[nmv] = hfdur[i];
			nmv++;
		}
	}
	stats(nmv,mdata,&qmean,&qmedian,&qstdv);

// DH[18]		High flow duration 1.  Compute the average duration of flow events with flows above a
//			threshold equal to three times the median flow value for the entire flow record.  DH[18] is the
//			average duration of the events.  (days)

	DH[18] = qmean;
	if(usemedian) DH[18] = qmedian;
	LDH[18] = percentile(lb,nmv,mdata);
	UDH[18] = percentile(ub,nmv,mdata);
	if(LDH[18] > DH[18]) LDH[18] = DH[18];
	if(UDH[18] < DH[18]) UDH[18] = DH[18];

	threshold = 7.*MA[2];
	for(i=0;i<nyrs;i++)
	{
		np[i] = 0.;			// Number of pulses
		hfdur[i] = 0.;		// Average duration of pulses for each year
		pdur = flag = 0;
		for(j=0;j<366;j++)
		{
			if(q[i][j] >= nearhuge) continue;
			if(q[i][j] > threshold)
			{
				pdur++;		// Compile number of days in all pulses
				flag++;
				if(flag==1) np[i]++;	// Add a pulse
			}
			else
			{
				flag = 0;
			}
		}
		if(np[i]>0.) hfdur[i] = pdur / np[i];	// Compute average duration
	}
	nmv = 0;
	for(i=0;i<nyrs;i++)
	{
		if(hfdur[i] > 0.)
		{
			mdata[nmv] = hfdur[i];
			nmv++;
		}
	}
	stats(nmv,mdata,&qmean,&qmedian,&qstdv);

// DH[19]		High flow duration 1.  Compute the average duration of flow events with flows above a
//			threshold equal to seven times the median flow value for the entire flow record.  DH[19] is the
//			average duration of the events.  (days)

	DH[19] = qmean;
	if(usemedian) DH[19] = qmedian;
	LDH[19] = percentile(lb,nmv,mdata);
	UDH[19] = percentile(ub,nmv,mdata);
	if(LDH[19] > DH[19]) LDH[19] = DH[19];
	if(UDH[19] < DH[19]) UDH[19] = DH[19];

// Compute high flow duration 2

// DH[20]		High flow duration 2.  Compute the 75th percentile value for the entire set of flows.
//			Compute the average duration of flow events with flows above a threshold equal to the 75th
//			percentile value for the entire set of flows.  DH[20] is the average duration of the events.
//			(days)

	order(1,data,ndv);		// Order data in ascending order
	threshold = percentile(.75,ndv,data);
	for(i=0;i<nyrs;i++)
	{
		np[i] = 0.;			// Number of pulses
		hfdur[i] = 0.;		// Average duration of pulses for each year
		pdur = flag = 0;
		for(j=0;j<366;j++)
		{
			if(q[i][j] >= nearhuge) continue;
			if(q[i][j] > threshold)
			{
				pdur++;		// Compile number of days in all pulses
				flag++;
				if(flag==1) np[i]++;	// Add a pulse
			}
			else
			{
				flag = 0;
			}
		}
		if(np[i]>0.) hfdur[i] = pdur / np[i];	// Compute average duration
	}
	nmv = 0;
	for(i=0;i<nyrs;i++)
	{
		if(hfdur[i] > 0.)
		{
			mdata[nmv] = hfdur[i];
			nmv++;
		}
	}
	stats(nmv,mdata,&qmean,&qmedian,&qstdv);
	DH[20] = qmean;
	if(usemedian) DH[20] = qmedian;
	LDH[20] = percentile(lb,nmv,mdata);
	UDH[20] = percentile(ub,nmv,mdata);
	if(LDH[20] > DH[20]) LDH[20] = DH[20];
	if(UDH[20] < DH[20]) UDH[20] = DH[20];

// DH[21]		High flow duration 2.  Compute the 25th percentile value for the entire set of flows.
//			Compute the average duration of flow events with flows above a threshold equal to the 75th
//			percentile value for the entire set of flows.  DH[21] is the average duration of the events.
//			(days)

	threshold = percentile(.25,ndv,data);
	for(i=0;i<nyrs;i++)
	{
		np[i] = 0.;			// Number of pulses
		hfdur[i] = 0.;		// Average duration of pulses for each year
		pdur = flag = 0;
		for(j=0;j<366;j++)
		{
			if(q[i][j] >= nearhuge) continue;
			if(q[i][j] > threshold)
			{
				pdur++;		// Compile number of days in all pulses
				flag++;
				if(flag==1) np[i]++;	// Add a pulse
			}
			else
			{
				flag = 0;
			}
		}
		if(np[i]>0.) hfdur[i] = pdur / np[i];	// Compute average duration
	}
	nmv = 0;
	for(i=0;i<nyrs;i++)
	{
		if(hfdur[i] > 0.)	// Exlclude years with no pulses
		{
			mdata[nmv] = hfdur[i];
			nmv++;
		}
	}
	stats(nmv,mdata,&qmean,&qmedian,&qstdv);
	DH[21] = qmean;
	if(usemedian) DH[21] = qmedian;
	LDH[21] = percentile(lb,nmv,mdata);
	UDH[21] = percentile(ub,nmv,mdata);
	if(LDH[21] > DH[21]) LDH[21] = DH[21];
	if(UDH[21] < DH[21]) UDH[21] = DH[21];

// Compute flood interval

// DH[22]		Flood interval.  Compute the flood threshold as the flow equivalent for a flood recurrence of
//			1.67 years.  Determine the median number of days between flood events for each year.
//			DH[22] is the mean of the yearly median number of days between flood events.  (days)

	if(dopeak)
	{
		threshold = (double)pow(10.,(double)lq167);
		for(i=0;i<nyrs;i++)
		{
			flag = 0;
			nmv = 0;
			pdur = 0;
			for(j=0;j<366;j++)
			{
				if(q[i][j] >= nearhuge) continue;
				if(q[i][j] < threshold)		// Begin non pulse period
				{
					flag++;
					pdur++;
					if(flag==1) nmv++;
				}
				else	// non pulse period has ended
				{
					if(flag > 0)
					{
						mdata[nmv-1] = pdur;		// Number of days for this interval
					}
					flag = 0;
					pdur = 0;
				}
			}
			if(nmv > 0)
			{
				stats(nmv,mdata,&qmean,&qmedian,&qstdv);
				np[i] = qmedian;		// Median interval between floods for the year
			}
			else
				np[i] = 0.;
		}
		stats(nyrs,np,&qmean,&qmedian,&qstdv);
		DH[22] = qmean;
		if(usemedian) DH[22] = qmedian;
		LDH[22] = percentile(lb,nyrs,np);
		UDH[22] = percentile(ub,nyrs,np);
//		if(LDH[22] > DH[22]) LDH[22] = DH[22];
//		if(UDH[22] < DH[22]) UDH[22] = DH[22];

// Compute flood duration 2

// DH[23]		Flood duration 2.  Compute the flood threshold as the flow equivalent for a flood recurrence of
//			1.67 years.  Determine the mean number of days each year that the flow remains above the
//			flood threshold.  DH[23] is the mean of the number of flood days for years in which floods occur.
//			(days)

		total = 0.;
		k = 0;
		for(i=0;i<nyrs;i++)
		{
			pdur = 0;
			np[i] = 0;

// Compute total number of days above threshold

			for(j=0;j<366;j++)
			{
				if(q[i][j] >= nearhuge) continue;
				if(q[i][j] > threshold)
				{
					pdur++;
				}
			}
			np[i] = (double)pdur;

			if(np[i] > 0.)	// Exclude years with no floods
			{
				mdata[k] = np[i];
				k++;
			}
		}
		stats(k,mdata,&qmean,&qmedian,&qstdv);
		DH[23] = qmean;
		if(usemedian) DH[23] = qmedian;
		LDH[23] = percentile(lb,k,mdata);
		UDH[23] = percentile(ub,k,mdata);
//		if(LDH[23] > DH[23]) LDH[23] = DH[23];
//		if(UDH[23] < DH[23]) UDH[23] = DH[23];

// Compute flood free days

		for(i=0;i<nyrs;i++)
		{
			flag = 0;
			nmv = 0;
			pdur = 0;
			for(j=0;j<366;j++)
			{
				if(q[i][j] >= nearhuge) continue;
				if(q[i][j] < threshold)		// Begin flood free period
				{
					flag++;
					pdur++;
					if(flag==1) nmv++;
				}
				else	// End of flood free period
				{
					if(flag > 0)
					{
						mdata[nmv-1] = pdur;		// Number of days for this interval
					}
					flag = 0;
					pdur = 0;
				}
			}
			if(nmv > 0)
			{
				np[i] = 0.;
				for(k=0;k<nmv;k++)
				{
					if(mdata[k] > np[i]) np[i] = mdata[k];
				}
			}
			else
				np[i] = 0.;
		}

		stats(nyrs,np,&qmean,&qmedian,&qstdv);

// DH[24]		Flood free days.  Compute the flood threshold as the flow equivalent for a flood recurrence of
//			1.67 years.  Compute the maximum number of days that the flow is below the threshold for each
//			year.  DH[24] is the mean of the maximum yearly no flood days.  (days)

		DH[24] = qmean;
		if(usemedian) DH[24] = qmedian;
		LDH[24] = percentile(lb,nyrs,np);
		UDH[24] = percentile(ub,nyrs,np);
//		if(LDH[24] > DH[24]) LDH[24] = DH[24];
//		if(UDH[24] < DH[24]) UDH[24] = DH[24];
	}


//***************************  Timing of flow events  *************************

/*	double TA1;			// Constancy
 *	double TA2;			// Predictability of flow
 *	double TA3;			// Seasonal predictability of flooding
 *
 *	double TL1;			// Julian date of annual minimum
 *	double TL2;			// Variability in Julian date of annual minimum
 *	double TL3;			// Seasonal predictability of low flow
 *	double TL4;			// Seasonal predictability of non-low flow
 *
 *	double TH1;			// Julian date of annual maximum
 *	double TH2;			// Variability in Julian date of annual maximum
 *	double TH3;			// Seasonal predictability of non-flooding */


//**************************  Average flow conditions  *************************

// Reference:  Colwell,R.k. 1974. Predictability, constancy, and contingency of periodic phenomena.
//             Ecology 55: 1148-1153.

// TA[1]	Constancy.  Constancy is computed via the formulation of Colwell.  A matrix of values is
//		compiled where the rows are 11 flow categories and the columns are 365 (no Feb 29th) days of
//		the year.  The cell values are the number of times that a flow falls into a category on each day.
//		The categories are:
//			log(flow) < .1 x log(mean flow),
//			.1 x log(mean flow) <= log(flow) < .25 x log(mean flow)
//			.25 x log(mean flow) <= log(flow) < .5 x log(mean flow)
//			.5 x log(mean flow) <= log(flow) < .75 x log(mean flow)
//			.75 x log(mean flow) <= log(flow) < 1.0 x log(mean flow)
//			1.0 x log(mean flow) <= log(flow) < 1.25 x log(mean flow)
//			1.25 x log(mean flow) <= log(flow) < 1.5 x log(mean flow)
//			1.5 x log(mean flow) <= log(flow) < 1.75 x log(mean flow)
//			1.75 x log(mean flow) <= log(flow) < 2.0 x log(mean flow)
//			2.0 x log(mean flow) <= log(flow) < 2.25 x log(mean flow)
//			log(flow) >= 2.25 x log(mean flow)
//
//		The row totals, column totals, and grand total are computed.  Using the equations for Shannon
//		information theory parameters, constancy is computed as 1 - (uncertainty with respect to state)
//										          log (number of states)
//		(dimensionless)
//		Limits are not computed.

// Compile Cowell table for computation of constancy and predictability

// Set eleven flow categories (rows) with break points at .1, .25, .5, .75, 1., 1.25,
// 1.5, 1.75, 2., and 2.25 times the log of the overall mean.
// Columns are 365 (skip Feb 29th) days of water year

	double lma1;				// Log10 of overall mean
	double lq;				// Log of daily flow value
	int cell[11][365];		// Cell values of Colwell table containing number of times
							// flows fell within flow categories for each day in year.
	int nrows = 11;			// Number of rows
	int ncol = 365;			// Number of columns


	lma1 = log10(MA[1]);

// Fill in the Colwell table

	for(i=0;i<nrows;i++)
	{
		for(j=0;j<ncol;j++)
		{
			cell[i][j] = 0;
		}
	}
	for(i=0;i<nyrs;i++)
	{
		k = 0;
		for(j=0;j<366;j++)
		{
			if(j==151 || q[i][j] >= nearhuge) continue;
			if(q[i][j] == 0.)
				lq = log10(.01);
			else
				lq = log10(q[i][j]);
			if(lq < .1*lma1) cell[0][k]++;
			if(lq >= .1*lma1 && lq < .25*lma1) cell[1][k]++;
			if(lq >= .25*lma1 && lq < .5*lma1) cell[2][k]++;
			if(lq >= .5*lma1 && lq < .75*lma1) cell[3][k]++;
			if(lq >= .75*lma1 && lq < lma1) cell[4][k]++;
			if(lq >= lma1 && lq < 1.25*lma1) cell[5][k]++;
			if(lq >= 1.25*lma1 && lq < 1.5*lma1) cell[6][k]++;
			if(lq >= 1.5*lma1 && lq < 1.75*lma1) cell[7][k]++;
			if(lq >= 1.75*lma1 && lq < 2.*lma1) cell[8][k]++;
			if(lq >= 2.*lma1 && lq < 2.25*lma1) cell[9][k]++;
			if(lq >= 2.25*lma1) cell[10][k]++;
			k++;
		}
	}

// Compute the Shannon information statistics

	double XJ[365];		// Column totals
	double YI[11];		// Row totals
	double Z;			// Grand total
	double HX;			// Uncertainty with respect to time
	double HY;			// Uncertainty with respect to state
	double HXY;			// Uncertainty with respect to the interaction of state and time
	double HxY;			// Conditional uncertainty with regard to state with time given

	for(i=0;i<nrows;i++)
	{
		YI[i] = 0;
		for(j=0;j<ncol;j++)
		{
			YI[i] = YI[i] + (double)cell[i][j];
		}
	}
	for(j=0;j<ncol;j++)
	{
		XJ[j] = 0;
		for(i=0;i<nrows;i++)
		{
			XJ[j] = XJ[j] + (double)cell[i][j];
		}
	}
	Z = 0;
	for(i=0;i<ncol;i++){Z = Z + XJ[i];}

	HX = 0;
	for(i=0;i<ncol;i++)
	{
		if(XJ[i] > 0) HX = HX + (XJ[i]/Z)*log10((double)XJ[i]/(double)Z);
	}
	HX = -HX;

	HY = 0;
	for(i=0;i<nrows;i++)
	{
		if(YI[i] > 0) HY = HY + (YI[i]/Z)*log10((double)YI[i]/(double)Z);
	}
	HY = -HY;

	HXY = 0;
	for(i=0;i<nrows;i++)
	{
		for(j=0;j<ncol;j++)
		{
			if(cell[i][j] > 0) HXY = HXY + (cell[i][j]/Z)*log10((double)cell[i][j]/(double)Z);
		}
	}
	HXY = -HXY;

	HxY = HXY - HX;

// TA[2]		Predictability.  Predictability is computed from the same matrix as constancy.  It is computed as:
//
//			1 - (uncertainty with respect to the interaction of time and state - uncertainty with respect to time) / log (number of states)
//			(dimensionless)
//			Limits are not computed.

	TA[2] = 100*(1 - (HxY/log10((double)nrows)));
	TA[1] = 1 - (HY/log10((double)nrows));


// Compute Seasonal predictability of flooding

// TA[3]		Seasonal predictability of flooding.  Divide years up into 2 month periods (i.e. Oct-Nov,
//			Dec-Jan, etc.).  Count the number of floods (flow events with flows > 1.67 year flood) in each
//			period over the entire flow record.  TA[3] is the maximum number of floods in any one period
//			divided by the total number of floods.  (dimensionless)

	int nfloods[6];		// Number of floods for each 60 day period (use 61 for 366 days)
	int tflds;			// Total number of floods for the entire record
	int maxf;			// Max number of floods in any 60 day period

	if(dopeak)
	{
		threshold = (double)pow(10.,(double)lq167);
		tflds = maxf = 0;
		nfloods[0] = nfloods[1] = nfloods[2] = nfloods[3] = nfloods[4] = nfloods[5] = 0;
		for(i=0;i<nyrs;i++)
		{
			flag = 0;
			for(j=0;j<366;j++)
			{
				if(q[i][j] >= nearhuge) continue;
				if(q[i][j] > threshold)
				{
					flag++;
					if(flag==1) tflds++;
					if(j>=0 && j<61)	// Oct - Nov
					{
						if(j==0) flag = 1;
						if(flag==1) nfloods[0]++;
					}
					if(j>=61 && j<122)	// Dec - Jan
					{
						if(j==61) flag = 1;
						if(flag==1) nfloods[1]++;
					}
					if(j>=122 && j<183)	// Feb - Mar
					{
						if(j==122) flag = 1;
						if(flag==1) nfloods[2]++;
					}
					if(j>=183 && j<244)	// Apr - May
					{
						if(j==183) flag = 1;
						if(flag==1) nfloods[3]++;
					}
					if(j>=244 && j<305)	// Jun - Jul
					{
						if(j==244) flag = 1;
						if(flag==1) nfloods[4]++;
					}
					if(j>=305 && j<366)	// Aug - Sep
					{
						if(j==305) flag = 1;
						if(flag==1) nfloods[5]++;
					}
				}
				else
				{
					flag = 0;
				}
			}
			for(k=0;k<6;k++)
			{
				if(nfloods[k] > maxf) maxf = nfloods[k];
			}
		}
		TA[3] = 999999;
		if(tflds > 0.) TA[3] = (double)maxf / (double)tflds;
	}


//**************************  Low flow conditions  *************************


// Compute Julian date of annual minimum

// TL[1]		Julian date of annual minimum.  Determine the Julian date (water year date + 274) that the
//			minimum flow occurs for each year.  Transform the dates to relative values on a circular
//			scale (radians or degrees).  Compute the x and y components for each year and average them
//			across all years.  Compute the mean angle as the arc tangent of ymean divided by xmean.
//			Transform the resultant angle back to Julian date.  (Julian day)
//			Limits are not computed.

// Formulas came from Julian Olden of CSU.


	double xbar, ybar;		// X and Y components of circular equivalent of Julian day
	int date[150];			// Julian day for annual minimum
	double pi = 3.141592654;
	double ratio;			// Ratio of Y to X component values

	for(i=0;i<nyrs;i++)
	{
		minaq[i] = 10000000.;
		for(j=0;j<366;j++)
		{
			if(q[i][j] < nearhuge)
			{
				if(minaq[i] > q[i][j])
				{
					minaq[i] = q[i][j];
					date[i] = j + 274;	// Convert to calendar year
					if(date[i] > 365) date[i] = date[i] - 365;
				}
			}
		}
	}
	for(i=0;i<nyrs;i++)
	{
		temp = (double)date[i]*2.*pi/365.25;
		np[i] = cos(temp);
		mdata[i] = sin(temp);
	}
	stats(nyrs,np,&xbar,&qmedian,&qstdv);
	stats(nyrs,mdata,&ybar,&qmedian,&qstdv);
	ratio = (double)ybar / (double)xbar;
	if(xbar > 0.) TL[1] = atan(ratio)*180./pi;
	if(xbar < 0.) TL[1] = atan(ratio)*180./pi + 180.;
	if(xbar==0. && ybar > 0.) TL[1] = 90.;
	if(xbar==0. && ybar < 0.) TL[1] = 270.;
	if(TL[1] < 0.) TL[1] = TL[1] + 360.;
	TL[1] = TL[1]*365.25/(360.);

// Compute variability in Julian date of annual minimum

// TL[2]		Variability in Julian date of annual minima.  Compute the coefficient of variation for the
//			mean x and y components and convert to a date.  (Julian days)
//			Limits are not computed.

// Formulas came from Julian Olden of CSU.

	temp = sqrt(xbar*xbar + ybar*ybar);
	temp = sqrt(2*(1-temp));
	TL[2] = temp*180/pi/360*365.25;

// Compute seasonal predictability of low flow

	int nlf[6];			// Number of low flow events for each 60 day period (use 61 for 366 days)
	int tnlf;			// Total number of low flow events for the entire record
	int maxlf;			// Max number of low flow events in any 60 day period

// Find the 5 year flood threshold

// 5-year low flow threshold (Poff, 1996). For TL[3] and TL[4] compute the log10 of the peak annual
// flows. Compute the log10 of the daily flows for the peak annual flow days. Calculate the
// coefficients for a linear regression equation for logs of peak annual flow versus logs of average
// daily flow for peak days. Using the log peak flow for the 5-year recurrence interval (80th
// percentile) as input to the regression equation, predict the log10 of the average daily flow.
// The threshold is 10 to the log10 (average daily flow) power (cubic feet per second).

	if(dopeak)
	{

// TL[3]		Seasonal predictability of low flow. Divide years up into 2-month periods (that is,
//			Oct-Nov, Dec-Jan, and so forth). Count the number of low flow events (flow events with
//			flows <= 5 year low flow threshold) in each period over the entire flow record. TL[3] is
//			the maximum number of low flow events in any one period divided by the total number of
//			low flow events (dimensionless - spatial).

		temp = percentile(.80,npyrs,lpeak);
		lq5 = a + b*temp;
		threshold = (double)pow(10.,(double)lq5);

		tnlf = 0;
		maxlf = 0;
		nlf[0] = nlf[1] = nlf[2] = nlf[3] = nlf[4] = nlf[5] = 0;
		for(i=0;i<nyrs;i++)
		{
			flag = 0;
			for(j=0;j<366;j++)
			{
				if(q[i][j] >= nearhuge) continue;
				if(q[i][j] <= threshold)
				{
					flag++;
					if(flag==1) tnlf++;
					if(j>=0 && j<61)	// Oct - Nov
					{
						if(j==0) flag = 1;
						if(flag==1) nlf[0]++;
					}
					if(j>=61 && j<122)	// Dec - Jan
					{
						if(j==61) flag = 1;
						if(flag==1) nlf[1]++;
					}
					if(j>=122 && j<183)	// Feb - Mar
					{
						if(j==122) flag = 1;
						if(flag==1) nlf[2]++;
					}
					if(j>=183 && j<244)	// Apr - May
					{
						if(j==183) flag = 1;
						if(flag==1) nlf[3]++;
					}
					if(j>=244 && j<305)	// Jun - Jul
					{
						if(j==244) flag = 1;
						if(flag==1) nlf[4]++;
					}
					if(j>=305 && j<366)	// Aug - Sep
					{
						if(j==305) flag = 1;
						if(flag==1) nlf[5]++;
					}
				}
				else
				{
					flag = 0;
				}
			}
			for(k=0;k<6;k++)
			{
				if(nlf[k] > maxlf) maxlf = nlf[k];
			}
		}
		TL[3] = 999999;
		if(tnlf > 0.) TL[3] = (double)maxlf / (double)tnlf;

// Compute seasonal predictability of non-low flow

// TL[4]		Seasonal predictability of non-low flow (i.e., above the low flow threshold). Count  the
//			number of days in each year that the flow is above the 5-year low flow threshold.
//			Compute  the ratio of number of days to 365 or 366 (leap year) for each year. TL[4] is
//			the maximum of the yearly ratios (dimensionless - spatial).

		TL[4] = 0;
		for(i=0;i<nyrs;i++)
		{
			day = 365;
			if(yr[i] % 4 == 0) day = 366;		// Leap year
			tnlf = 0;
			for(j=0;j<366;j++)
			{
				if(q[i][j] >= nearhuge) continue;
				if(q[i][j] > threshold) tnlf++;
			}
			temp = (double)tnlf / (double)day;
			if(temp > TL[4]) TL[4] = temp;
		}
	}

//**************************  High flow conditions  *************************


// Compute Julian date of annual maximum

// TH[1]		Julian date of annual maximum.  Determine the Julian date (water year date + 274) that the
//			maximum flow occurs for each year.  Transform the dates to relative values on a circular
//			scale (radians or degrees).  Compute the x and y components for each year and average them
//			across all years.  Compute the mean angle as the arc tangent of ymean divided by xmean.
//			Transform the resultant angle back to Julian date.  (Julian day)
//			Limits are not computed.

// Formulas came from Julian Olden of CSU.

	for(i=0;i<nyrs;i++)
	{
		maxaq[i] = 0.;
		for(j=0;j<366;j++)
		{
			if(q[i][j] < nearhuge)
			{
				if(maxaq[i] < q[i][j])
				{
					maxaq[i] = q[i][j];
					date[i] = j + 274;	// Convert to calendar year
					if(date[i] > 365) date[i] = date[i] - 365;
				}
			}
		}
	}
	for(i=0;i<nyrs;i++)
	{
		temp = (double)date[i]*2.*pi/365.25;
		np[i] = cos(temp);
		mdata[i] = sin(temp);
	}
	stats(nyrs,np,&xbar,&qmedian,&qstdv);
	stats(nyrs,mdata,&ybar,&qmedian,&qstdv);
	ratio = (double)ybar / (double)xbar;
	if(xbar > 0.) TH[1] = atan(ratio)*180./pi;
	if(xbar < 0.) TH[1] = atan(ratio)*180./pi + 180.;
	if(xbar==0. && ybar > 0.) TH[1] = 90.;
	if(xbar==0. && ybar < 0.) TH[1] = 270.;
	if(TH[1] < 0.) TH[1] = TH[1] + 360.;
	TH[1] = TH[1]*365.25/(360.);

// Compute variability in Julian date of annual maximum

// TH[2]		Variability in Julian date of annual maxima.  Compute the coefficient of variation for the
//			mean x and y components and convert to a date.  (Julian days)
//			Limits are not computed.

// Formulas came from Julian Olden of CSU.

	temp = sqrt(xbar*xbar + ybar*ybar);
	temp = sqrt(2*(1-temp));
	TH[2] = temp*180/pi/360*365.25;

// Compute seasonal predictability of non flooding

// TH[3]		Seasonal predictability of non-flooding. Identify  non-flow days (days when the flow is
//			less than the 1.67-year bankfull threshold)   that occur on the same date for all years
//			in the period of record.  Accumulate consective  days that meet this criterion as
//			flood-free periods.  TH[3] is maximum length of those flood-free periods divided by
//			365 (dimensionless - spatial).

	int nfdur[365];			// No flood durations across all years
	int nnflds;				// Number of no flood periods across all years

	if(dopeak)
	{
		threshold = (double)pow(10.,(double)lq167);
		TH[3] = 0.;
		nnflds = 0;
		for(i=0;i<365;i++){nfdur[i] = 0;}
		flag = 0;
		for(j=0;j<366;j++)
		{
			tnlf = 0;
			for(i=0;i<nyrs;i++)
			{
				if(q[i][j] >= nearhuge) continue;
				if(q[i][j] > threshold) tnlf++;
			}
			if(tnlf == 0)
			{
				flag++;
				if(flag == 1) nnflds++;
				nfdur[nnflds-1]++;
			}
			else
			{
				flag = 0;
			}
		}
		maxlf = 0.;
		for(i=0;i<nnflds;i++)
		{
			if(nfdur[i] > maxlf) maxlf = nfdur[i];
		}
		TH[3] = (double)maxlf / 365;
	}


//**************************  Rate of change in flow events  *********************

/*	double RA1, LRA1, URA1;	// Rise rate
 *	double RA2;				// Variability in rise rate
 *	double RA3, LRA3, URA3;	// Fall rate
 *	double RA4;				// Variability in fall rate
 *	double RA5;				// No day rises
 *	double RA6, LRA6, URA6;	// Change of flow (increasing)
 *	double RA7, LRA7, URA7;	// Change of flow (descreasing)
 *	double RA8, LRA8, URA8;	// Number of reversals
 *	double RA9;				// Variability in reversals */

// Compute rise rate

// RA[1]		Rise rate.  Compute the change in flow for days in which the change is positive for the
//			entire flow record.  RA[1] is the mean of these values.  (cfs/day)

// Recompile the data

	ndv = 0;
	for(i=0;i<nyrs;i++)
	{
		for(j=0;j<366;j++)
		{

			if(q[i][j]  < nearhuge)
			{

//			printf("yearno: %i  dayno: %i  Q: %12.3f\n", i, j, q[i][j]);
				data[ndv] = q[i][j];
				ndv++;
			}
		}
	}

	nmv = 0;
	for(i=0;i<ndv-1;i++)
	{
		temp = data[i+1] - data[i];	// Change in flow
		if(temp > 0.)
		{
			mdata[nmv] = temp;
			nmv++;
		}
	}
	stats(nmv,mdata,&qmean,&qmedian,&qstdv);
	RA[1] = qmean;
	if(usemedian) RA[1] = qmedian;
	LRA[1] = percentile(lb,nmv,mdata);
	URA[1] = percentile(ub,nmv,mdata);
	if(LRA[1] > RA[1]) LRA[1] = RA[1];
	if(URA[1] < RA[1]) URA[1] = RA[1];

// Compute variability in rise rate

// RA[2]		Variability in rise rate.  Compute the standard deviation for the positive flow changes.
//			RA[2] is 100 times the standard deviation divided by the mean.  (%)
//			Limits are not computed.

	RA[2] = 999999;
	if(qmean != 0.) RA[2] = 100.*qstdv / qmean;

// Compute no day rises

// RA[5]		Number of day rises.  Compute the number of days in which the flow is greater than the
//			previous day.  RA[5] is the number of positive gain days divided by the total number of
//			days in the flow record.  (dimensionless)
//			Limits are not computed.

	RA[5] = (double)nmv / (double)ndv;

// Compute fall rate

// RA[3]		Fall rate.  Compute the change in flow for days in which the change is negative for the
//			entire flow record.  RA[3] is the mean of these values.  (cfs/day)

	nmv = 0;
	for(i=0;i<ndv-1;i++)
	{
		temp = data[i+1] - data[i];	// Change in flow
		if(temp < 0.)
		{
			mdata[nmv] = fabs(temp);
			nmv++;
		}
	}
	stats(nmv,mdata,&qmean,&qmedian,&qstdv);
	RA[3] = qmean;
	if(usemedian) RA[3] = qmedian;
	LRA[3] = percentile(lb,nmv,mdata);
	URA[3] = percentile(ub,nmv,mdata);
	if(LRA[3] > RA[3]) LRA[3] = RA[3];
	if(URA[3] < RA[3]) URA[3] = RA[3];

// Compute variability in fall rate

// RA[4]		Variability in fall rate.  Compute the standard deviation for the negative flow changes.
//			RA[4] is 100 times the standard deviation divided by the mean.  (%)
//			Limits are not computed.

	RA[4] = 999999;
	if(qmean != 0.) RA[4] = 100.*qstdv / qmean;

// Compute change of flow (increasing and descreasing)

// RA[6]		Change of flow.  Compute the log10 of the flows for the entire flow record.  Compute the change
//			in log of flow for days in which the change is positive for the entire flow record.  RA[6] is the
//			median of these values.  (cfs/day)

	ndv = 0;
	for(i=0;i<nyrs;i++)
	{
		for(j=0;j<366;j++)
		{
			if(q[i][j]  < nearhuge)
			{
				if(q[i][j] > 0.)
					data[ndv] = log(q[i][j]);
				else
					data[ndv] = log(.01);
				ndv++;
			}
		}
	}

	nmv = 0;
	for(i=0;i<ndv-1;i++)
	{
		temp = data[i+1] - data[i];
		if(temp > 0.)
		{
			mdata[nmv] = temp;
			nmv++;
		}
	}
	stats(nmv,mdata,&qmean,&qmedian,&qstdv);
	RA[6] = qmedian;
	LRA[6] = percentile(lb,nmv,mdata);
	URA[6] = percentile(ub,nmv,mdata);
	if(LRA[6] > RA[6]) LRA[6] = RA[6];
	if(URA[6] < RA[6]) URA[6] = RA[6];

// RA[7]		Change of flow.  Compute the log10 of the flows for the entire flow record.  Compute the change
//			in log of flow for days in which the change is negative for the entire flow record.  RA[7] is the
//			median of these values.  (cfs/day)

	nmv = 0;
	for(i=0;i<ndv-1;i++)
	{
		temp = data[i+1] - data[i];
		if(temp < 0.)
		{
			mdata[nmv] = fabs(temp);
			nmv++;
		}
	}
	stats(nmv,mdata,&qmean,&qmedian,&qstdv);
	RA[7] = qmedian;
	LRA[7] = percentile(lb,nmv,mdata);
	URA[7] = percentile(ub,nmv,mdata);
	if(LRA[7] > RA[7]) LRA[7] = RA[7];
	if(URA[7] < RA[7]) URA[7] = RA[7];

// Compute number of reversals

// RA[8]		Number of reversals.  Compute the number of days in each year when the change in flow from
//			one day to the next changes direction.  RA[8] is the average of the yearly values.  (days)

	for(i=0;i<nyrs;i++)
	{
		np[i] = 0.;
		nmv = 0;
		for(j=0;j<366;j++)
		{
			if(q[i][j]  < nearhuge)
			{
				mdata[nmv] = q[i][j];
				nmv++;
			}
		}
		for(j=0;j<nmv-1;j++)
		{
			temp = mdata[j+1] - mdata[j];
			if(j==0)
			{
				flag = 0;
				if(temp > 0.) flag = 1;
				if(temp < 0.) flag = 2;
			}
			if(j > 0 && temp > 0.)
			{
				if(flag == 2)		// Previous change was negative
				{
					np[i]++;
				}
				flag = 1;
			}
			if(j > 0 && temp < 0.)
			{
				if(flag == 1)		// Previous change was positive
				{
					np[i]++;
				}
				flag = 2;
			}
		}
	}
	stats(nyrs,np,&qmean,&qmedian,&qstdv);
	RA[8] = qmean;
	if(usemedian) RA[8] = qmedian;
	LRA[8] = percentile(lb,nyrs,np);
	URA[8] = percentile(ub,nyrs,np);
	if(LRA[8] > RA[8]) LRA[8] = RA[8];
	if(URA[8] < RA[8]) URA[8] = RA[8];

// Compute variability in reversals

// RA[9]		Variability in reversals.  Compute the standard deviation for the yearly reversal values.
//			RA[9] is 100 times the standard deviation divided by the mean.  (%)
//			Limits are not computed.

	RA[9] = 0.;
	if(qmean != 0.) RA[9] = 100.*qstdv / qmean;

	free(data);
	free(mdata);
	free(baseflow);
}

// Compute basic statistics

void stats(int ndv, double *data, double *v1, double *v2, double *v3)
{
	int i, j;
	double total, temp;
	double dtmp;
	int icen;		// Center index for curve points

	*v1 = *v2 = *v3 = 0.;
	if(ndv == 0) return;

// Compute the mean

	total = 0.;
	for(i=0;i<ndv;i++)
	{
		total = total + (double)data[i];
	}
	*v1 = (double)(total / (double)ndv);
	temp = total / (double)ndv;

// compute the standard deviation

	total = 0.;
	for(i=0;i<ndv;i++)
	{
		total = total + ((double)data[i]-temp) * ((double)data[i]-temp);
	}
	*v3 = (double)sqrt(total/(double)(ndv-1));

// Compute the median

// Sort the Y values in ascending order

	for(i=0;i<ndv-1;i++)
	{
		for(j=0;j<ndv-1;j++)
		{
			if(data[j] > data[j+1])
			{
				dtmp = data[j];
				data[j] = data[j+1];
				data[j+1] = dtmp;
			}
		}
	}
	icen = (ndv+1) / 2 - 1;
	if(ndv % 2 == 0)
		*v2 = (data[icen] + data[icen+1]) / 2.;
	else
		*v2 = data[icen];
}

double percentile(double p, int ndv, double *data)
{
    int lo, hi;
    double d;
    double value;
    double yp;              // Percentile value
    
    yp = 0.;
    if(ndv == 0) return(yp);
    
    value = p * (ndv-1);
    lo = floor(value);
    hi = ceil(value);
    d = value - lo;
    
    if(lo==0)
        yp = data[0];
    else if(lo==ndv)
        yp = data[ndv-1];
    else
        yp = (1.0 - d) * data[lo] + d * data[hi];
    return(yp);
}

/////////////////////////////////////////////////////////////////////////////
//	order - Sorts in ascending or descending order for values for a data set
//
void order(int flag, double *ydata, int nval)
{
	int i, j;				//	Loop counters
	double tmp;

	if(flag == 0)
	{

// Sort the y values in descending order

		for(i=0;i<nval-1;i++)
		{
			for(j=0;j<nval-1;j++)
			{
				if(ydata[j] < ydata[j+1])
				{
					tmp = ydata[j];
					ydata[j] = ydata[j+1];
					ydata[j+1] = tmp;
				}
			}
		}
	}
	else
	{

// Sort the y values in ascending order

		for(i=0;i<nval-1;i++)
		{
			for(j=0;j<nval-1;j++)
			{
				if(ydata[j] > ydata[j+1])
				{
					tmp = ydata[j];
					ydata[j] = ydata[j+1];
					ydata[j+1] = tmp;
				}
			}
		}
	}
}
