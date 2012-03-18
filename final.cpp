//This code is distributed under the BSD 
//license and it is a rewrite of code shared 
//in class CSC431 at DePaul University 
//by Massimo Di Pierro
//Rewritten by Chengwei Chang & Weiguo Yan


#include <iostream>
#include <vector>
#include <algorithm>
#include <typeinfo>
#include <string>
#include <stdio.h>
#include <time.h>
#include<math.h>
using namespace std;
class Matrix
{
public:
	int rows;
	int cols;
	vector<double> data;
	Matrix(int rows, int cols) {
		this->rows=rows;
		this->cols=cols;
		this->data.resize(rows*cols);
		for(int r=0;r<rows;r++)
			for(int c=0;c<cols;c++)
				this->data[r*cols+c]=0;
	}
	double operator () (int i, int j) const {
		return data[i*cols+j];
	}
	double &operator () (int i, int j)  {
		return data[i*cols+j];
	}
};
istream &operator >> (istream &in,Matrix& A) {
	cout<<"input the elements: ";
	for(int r=0;r<A.rows;r++) 
		for(int c=0;c<A.cols;c++)
			in>>A.data[r*A.cols+c];
	return in;
}
ostream &operator << (ostream &out,const Matrix& A) {
	for(int r=0;r<A.rows;r++) {
		out << "|";
		for(int c=0;c<A.cols;c++) {
			if(c>0) out << " ";
		out << A(r,c);
		}
		out << "|\n";
	}
	out << "\n";
	return out;
}
Matrix operator + (const Matrix &A,Matrix &B) {
	if(A.rows!=B.rows || A.cols!=B.cols)
		cout << "BAD\n";
	Matrix C(A.rows,A.cols);
	for(int r=0;r<A.rows;r++)
		for(int c=0;c<A.cols;c++)
			C(r,c) = A(r,c) + B(r,c);
		return C;
}

Matrix operator + (const Matrix &A, double b) {
	Matrix B=A;
	if(B.rows==1)
		for(int c=0;c<B.cols;c++)
			B(0,c)=B(0,c)+b;
	else if(B.cols==1)
		for(int r=0;r<B.rows;r++)
			B(r,0)=B(r,0)+b;
	else
		for(int r=0;r<B.rows;r++)
			B(r,r)=B(r,r)+b;
	return B;
}


Matrix operator - (const Matrix &A,Matrix &B) {
	if(A.rows!=B.rows || A.cols!=B.cols)
		cout << "BAD\n";
	Matrix C(A.rows,A.cols);
	for(int r=0;r<A.rows;r++)
		for(int c=0;c<A.cols;c++)
			C(r,c) = A(r,c) - B(r,c);
		return C;
}

Matrix operator - (const Matrix &A, double b) {
	Matrix B=A;
	if(B.rows==1)
		for(int c=0;c<B.cols;c++)
			B(0,c)=B(0,c)-b;
	else if(B.cols==1)
		for(int r=0;r<B.rows;r++)
			B(r,0)=B(r,0)-b;
	else
		for(int r=0;r<B.rows;r++)
			B(r,r)=B(r,r)-b;
	return B;
}

Matrix operator - (Matrix A){
	Matrix B(A.rows,A.cols);
		for(int r=0; r<A.rows; r++)
			for(int c=0; c<A.cols; c++){
				if (A(c,r)!=0)
					B(r,c)=-A(c,r);
			}
		return B;
}

Matrix operator - (double b,const Matrix &A){
	Matrix B = -A;
	Matrix C = B +b;
	return C;
}


Matrix operator * (double a,Matrix &A) {
	Matrix C(A.rows,A.cols);
	for(int r=0;r<A.rows;r++)
		for(int c=0;c<A.cols;c++)
			C(r,c) = a*A(r,c);
		return C;
}


Matrix operator * (const Matrix &A,Matrix &B) {
	Matrix C(A.rows,B.cols);
	if(A.cols==1&&B.cols==1&A.rows==B.rows)
		for(int r=0;r<A.rows;r++)
			C(r,0)=A(r,0)*B(r,0);
	else
		if(A.cols!=B.rows)
			cout << "BAD\n";
		else
			for(int r=0;r<A.rows;r++)
				for(int c=0;c<B.cols;c++)
					for(int k=0;k<A.cols;k++)
						C(r,c) += A(r,k)*B(k,c);
	return C;
}

Matrix operator /(Matrix &A,double a) {
	Matrix C(A.rows,A.cols);
	for(int r=0;r<A.rows;r++)
		for(int c=0;c<A.cols;c++)
			C(r,c) = A(r,c)*(1/a);
		return C;
}



void swap(double&a, double &b) {
  double c=a; a=b; b=c;
}

Matrix inv(Matrix A) {
  if(A.cols!=A.rows)
    cout << "BAD\n";
  Matrix B(A.rows,A.cols);
  double p;
  double q;
  int m;
  for(int r=0; r<B.cols;r++) B(r,r)=1;
  for(int c=0; c<A.cols;c++) {    
    m=c; p=A(c,c);
    for(int i=c+1; i<A.rows; i++)
      if(abs(A(i,c)) > abs(p)) {m=i; p=A(i,c);}
    for(int i=0; i<A.cols; i++) {
      swap(A(m,i),A(c,i));
      swap(B(m,i),B(c,i));
    }
    for(int i=0; i<A.cols; i++) {
      A(c,i) /= p; 
      B(c,i) /= p;
    }
    for(int r=0; r<A.rows; r++) 
      if(r!=c) {
	q = A(r,c);
	for(int i=0; i<A.cols; i++) {
	  A(r,i)-=q*A(c,i);
	  B(r,i)-=q*B(c,i);
	}
      }
  }
  return B;
}

Matrix row(Matrix A, int i){
	Matrix B(1,A.cols);
		for (int c=0;c<A.cols;c++)
			B(0,c)=A(i-1,c);
		return B;
}
Matrix col(Matrix A, int i){
	Matrix B(A.rows,1);
		for (int r=0; r<A.rows;r++)
			B(r,0)=A(r,i-1);
		return B;
}
Matrix tran(Matrix A){
	Matrix B(A.cols,A.rows);
		for(int r=0; r<B.rows; r++)
			for(int c=0; c<B.cols; c++)
				B(r,c)=A(c,r);
		return B;
}


double norm(Matrix A,int p=1){
	vector<double>co;
	double s = 0;
	if(A.rows==1||A.cols==1){
		double x=0;
		for(int c=0; c<A.cols; c++){
			for(int r=0; r<A.rows; r++){
				x+=abs(A(r,c));
			}
		}
		return pow(x,1/p);
	}
	else if(p==1){
		double s = 0;
		for(int c=0; c<A.cols; c++){
			for(int r=0; r<A.rows; r++){
				s += abs(A(r,c));
			}
		co.insert(co.begin(),s);
		s = 0;
		}
		sort(co.begin(),co.end());
		return co[A.cols-1];
	}
	else
		throw string("error");
}

double norm(double A, int p=1){
	return abs(A);
}
double norm(vector<double>&A, int p=1){
	double x=0;
	for(int i=0;i<A.size();i++)
		x+=pow(A[i],p);
	return pow(x,1/p);
}


double max(double a,double b){
	if(a>b) return a;
return b;
}

bool is_almost_symmetric(Matrix A){
	if(A.rows!=A.cols)
	return false;
	double ap=1e-6;
	double rp=1e-4;
	for(int r=0; r<A.rows-1; r++)
		for(int c=0; c<A.rows; c++){
			double delta = abs(A(r,c)-A(c,r));
			if(delta>ap &&delta>max(abs(A(r,c)),abs(A(c,r)))*rp)
				return false;
		}
	return true;
}

bool is_almost_zero(Matrix A){
	double ap=1e-6;
	double rp=1e-4;
	for(int r=0; r<A.rows; r++)
		for(int c=0; c<A.cols; c++){
			double delta = abs(A(r,c)-A(c,r));
			if(delta>ap &&delta>max(abs(A(r,c)),abs(A(c,r)))*rp)
				return false;
		}
	return true;
}

double f(double x){
	return x*x-5.0*x;
}

double Df(double x, double h=1e-6){
	return (f(x+h)-f(x-h))/(2*h);
}

double condition_number(Matrix A){
	if(typeid(A)==typeid(Matrix))
		return norm(A)*norm(inv(A));
}



Matrix Identity(int rows=1, double one=1, double fill=0){
	Matrix M(rows,rows);
	for(int r=0; r<M.rows; r++)
		M(r,r)=one;
	return M;
}
Matrix exp_matrix(Matrix A,double ap=1e-6,double rp=1e-4,int ns=40){
	Matrix t =Identity(A.cols);
	Matrix s =Identity(A.cols);
	for(int k=1; k<ns; k++){
		t = t*A/k;
		s = s + t;
		if (norm(t)<max(ap,norm(s)*rp))
			return s;
	}
	throw string("no convergence");
}

double exp_number(double a){
	return exp(a);
}


Matrix Cholesky(Matrix A){
	if(is_almost_symmetric(A)== false)
		throw string("not is_almost_symmetric");
	Matrix B = A;
	for(int k=0; k<B.cols; k++){
		if (B(k,k)<=0)
			throw string("not positive definitive");
		double p=sqrt(B(k,k));
		B(k,k)=sqrt(B(k,k));
		for(int i=k+1; i<B.rows;i++)
			B(i,k)/=p;
		for(int j=k+1; j<B.rows;j++){
			p = B(j,k);
			for(int m=k+1; m<B.rows;m++)
				B(m,j) -= p*B(m,k);
		}
	}
	for(int n=0; n<B.rows;n++){
		for(int q=n+1;q<B.rows;q++)
			B(n,q)=0;
	}
	return B;
}

void is_positive_definite(Matrix A){
	if (is_almost_symmetric(A) == false)
		cout<<"is not positive definite"<<endl;
	else
		try{Cholesky(A);
			cout<<"is positive definite"<<endl;}
		catch(...){
			cout<<"is not positive definite"<<endl;
		}
}


Matrix portfolioOfMarkovitz(Matrix mu, Matrix A, double r_free){
	Matrix x(A.rows,1);
	x = (inv(A))*(mu - r_free);
	double s = 0;
	for(int r=0; r<x.rows; r++)
		s += x(r,0);
	x = x/s;
	Matrix portfolio(x.rows, 1);
	for(int r=0; r<x.rows; r++)
	portfolio(r,0) = x(r,0);
	return portfolio;
}

double returnOfMarkovitz(Matrix mu, Matrix A, double r_free){
	Matrix x(A.rows,1);
	x = (inv(A))*(mu - r_free);
	double s = 0;
	for(int r=0; r<x.rows; r++)
		s += x(r,0);
	x = x/s;
	Matrix return_portfolio(x.rows,1);
	return_portfolio = mu*x;
	double rtn=0;
	for(int r=0; r<return_portfolio.rows; r++)
		rtn +=return_portfolio(r,0);
	return rtn;
}

double riskOfMarkovitz(Matrix mu, Matrix A, double r_free){
	Matrix x(A.rows,1);
	x = (inv(A))*(mu - r_free);
	double s = 0;
	for(int r=0; r<x.rows; r++)
		s += x(r,0);
	x = x/s;
	double risk=0;
	Matrix n(A.rows,x.cols);
	n = A*x;
	Matrix m(x.rows,n.cols);
	m = x*n;
	for(int r=0; r<m.rows; r++)
		risk +=m(r,0);
	risk = sqrt(risk);
	return risk;
}



void fit_least_squares(float array[5][3], int a=4)
{
	Matrix A(5,4);
	Matrix b(5,1);
	for(int i=0;i<A.rows;i++)
	{
		float weight = 1.0/array[i][2];
		b(i,0) = weight*array[i][1];
		for(int j=0;j<A.cols;j++)
			A(i,j) = weight*pow(array[i][0],j);
	}
	cout<<inv(tran(A)*A)*(tran(A)*b);

}



class Function{
public :
	virtual double f(double x)=0;

	double Df(double x, double h=1e-6){
		return (f(x+h)-f(x-h))/(2*h);
}
	double DDf(double x, double h=1e-5){
		return (f(x+h)-2*f(x)+f(x-h))/(h*h);
	}
	
	double g(double x){
		return f(x)/100+x;
	}

	double Dg(double x, double h=1e-6){
		return (g(x+h)-g(x-h))/(2*h);
}
	double solve_newton(double x_guess, double ap=1e-6, double rp=1e-4, int ns=100){
		double x_old, x = x_guess;
		for(int k=0; k<ns;k++){
			x_old = x;
			x = x - f(x)/Df(x);
			if(k>2 && abs(x-x_old)<max(ap,rp*abs(x))) return x;
		}
	    throw string("no convergence");
	}
	
	double solve_fixed_point(double x_guess,double ap=1e-6, double rp=1e-4, int ns=100){
		double x_old, x = x_guess;
		for(int k=0; k<ns;k++){
			if((abs(Dg(x)))>=1)
				throw string("error D(g)(x)>=1");
			x_old =x;
			x = g(x);
			if(k>2 && abs(x-x_old)<max(ap,rp*abs(x))) return x;
		}
		throw string("no convergence");		
	}


	double solve_bisection(double a, double b, double ap=1e-6, double rp=1e-4, int ns=100){
		if(f(a)==0) return a;
		if(f(b)==0) return b;
		if(f(a)*f(b)>0) 
			throw string("f(a) and f(b) must have opposite sign");	
		double x=0;
		for(int k=0; k<ns;k++){
			x = (a+b)/2;
			if(f(x)==0 ||(abs(b-a))<max(ap,rp*abs(x))) return x;
			else if(f(x)*f(a)<0)
				b = x;
			else
				a = x;
		}
	    throw string("no convergence");
	}

	double solve_secant(double x, double ap=1e-6, double rp=1e-4, int ns=100){
		
		for(int k=0; k<ns;k++){
			if((abs(Df(x)))<ap)
				throw string("unstable solution");	
			double x_old=0;
			double fx_old =0;
			x_old = x;
			fx_old = f(x);
			x = x - f(x)/Df(x);
			if(k>2 && abs(x-x_old)<max(ap,rp*abs(x))) return x;
		}
	    throw string("no convergence");
	}

	double solve_newton_stabilized(double a, double b, double ap=1e-6, double rp=1e-4, int ns=100){
		if(f(a)==0) return a;
		if(f(b)==0) return b;
		if(f(a)*f(b)>0) 
			throw string("f(a) and f(b) must have opposite sign");	
		double x=0;
		x = (a+b)/2;
		double x_old=0;
		double fx_old =0;
		for(int k=0; k<ns;k++){
			x_old = x;
			fx_old = f(x);
			if((abs(Df(x)))>ap)
				x = x - f(x)/Df(x);
			if(x == x_old || x<a || x>b)
				x = (a+b)/2;
			if(f(x)==0 ||(abs(b-a))<max(ap,rp*abs(x))) return x;
			else if(f(x)*f(a)<0)
				b = x;
			else
				a = x;
		}
	    throw string("no convergence");
	}


	double optimize_newton(double x_guess, double ap=1e-6, double rp=1e-4, int ns=100){
		double x_old, x = x_guess;
		for(int k=0; k<ns;k++){
			x_old = x;
			x = x - Df(x)/DDf(x);
			if(k>2 && abs(x-x_old)<max(ap,rp*abs(x))) return x;
		}
	    throw string("no convergence");
	}

	double optimize_bisection(double a, double b, double ap=1e-6, double rp=1e-4, int ns=100){
		if(Df(a)==0) return a;
		if(Df(b)==0) return b;
		if(Df(a)*f(Df(b))>0) 
			throw string("Df(a) and Df(b) must have opposite sign");	
		double x=0;
		for(int k=0; k<ns;k++){
			x = (a+b)/2;
			if(Df(x)==0 ||(abs(b-a))<max(ap,rp*abs(x))) return x;
			else if(Df(x)*Df(a)<0)
				b = x;
			else
				a = x;
		}
	    throw string("no convergence");
	}

	double optimize_secant(double x, double ap=1e-6, double rp=1e-4, int ns=100){
		
		for(int k=0; k<ns;k++){
			if(Df(x)==0) return x;
			if((abs(DDf(x)))<ap)
				throw string("unstable solution");	
			double x_old=0;
			double Dfx_old =0;
			x_old = x;
			Dfx_old = Df(x);
			x = x - Df(x)/DDf(x);
			if(k>2 && abs(x-x_old)<max(ap,rp*abs(x))) return x;
		}
	    throw string("no convergence");
	}

	double optimize_newton_stabilized(double a, double b, double ap=1e-6, double rp=1e-4, int ns=100){
		if(Df(a)==0) return a;
		if(Df(b)==0) return b;
		if(Df(a)*Df(b)>0) 
			throw string("Df(a) and Df(b) must have opposite sign");	
		double x=0;
		x = (a+b)/2;
		double x_old=0;
		double fx_old =0;
		double Dfx_old =0;
		for(int k=0; k<ns;k++){
			if(Df(x)==0) return x;
			x_old = x;
			fx_old = f(x);
			Dfx_old = Df(x);
			if((abs(DDf(x)))>ap)
				x = x - Df(x)/DDf(x);
			if(x == x_old || x<a || x>b)
				x = (a+b)/2;
			if((abs(x-x_old))<max(ap,(abs(x))*rp)) return x;
			else if(Df(x)*Df(a)<0)
				b = x;
			else
				a = x;
		}
	    throw string("no convergence");
	}

	double optimize_golden_search(double a, double b, double ap=1e-6, double rp=1e-4, int ns=100){
		double tau = (sqrt(5.0)-1.0)/2.0;
		double x1 = a+(1.0-tau)*(b-a);
		double x2 = a+tau*(b-a);
		for(int k=0; k<ns;k++){
			if(f(x1)>f(x2)){
				a = x1;
				x1 =x2;
			    x2 = a+tau*(b-a);
			}
			else {
				b =x2;
				x2 =x1;
				x1 = a+(1.0-tau)*(b-a);
			}
			if( k>2 && abs(b-a)<max(ap,abs(b)*rp)) return b;
		}
		throw string("no convergence");
	}
};

class MyFunction : public Function{
	double f(double x){
		return (x-2)*(x-5);
	}
};

class MultiVariableFunction{
public:
	virtual double f(double *a)=0;
	double partial(int i, int s,double *a,double h=1e-4)
	{
		double u,v;
		for(int k=0;k<s;k++)
		{
			if(k==i)
			{
				a[i]+=h;
				u = f(a);
				a[i]-=2*h;
				v = f(a);
			}
		}
		return (u-v)/(2*h);
	}

	void gradient(int s,double *a)
	{
		Matrix G(s,1);
		for(int i=0;i<s;i++)
			G(i,0)=partial(i,s,a);
		cout<<G;
	}

};

class MyOtherFunction:public MultiVariableFunction
{
	
	double f(double *a)
	{
		return 2*(*a)+3*(*(a+1))+5*(*(a+1))*(*(a+2));
	}
};

void testOfMatrix(){
	// test for Class Matrix and its use
	int r,c;
	cout<<"input the rows and columns of Matrix A:";
	cin>>r>>c;
	Matrix A(r,c);
	cin>>A;
	cout<<A<<endl;
	cout<<"input the rows and columns of Matrix B:";
	cin>>r>>c;
	Matrix B(r,c);
	cin>>B;
	cout<<B<<endl;
	cout<<A+B<<endl;
	cout<<A+2<<endl;
	cout<<A-2<<endl;
	cout<<5-A<<endl;
	cout<<A-B<<endl;
	cout<<-A<<endl;   
	cout<<A*B<<endl;
	cout<<2*A<<endl;
	cout<<A/3<<endl;
	cout<<inv(A)<<endl;
	cout<<row(A,2)<<endl;
	cout<<col(A,2)<<endl;
	cout<<tran(A)<<endl;
	if (is_almost_symmetric(A) == true)
		cout<<"true"<<endl;
	else 
		cout<<"false"<<endl;
	
	if (is_almost_zero(A) == true)
		cout<<"true"<<endl;
	else 
		cout<<"false"<<endl;  
	cout<<norm(A)<<endl;
	cout<<condition_number(A)<<endl;
	cout<<exp_matrix(A)<<endl;
	cout<<exp_number(5)<<endl;
	Matrix M =Identity(9,1,0);
	cout<<M<<endl;
	cout<<Cholesky(A)<<endl;
	is_positive_definite(A);
	cout<<portfolioOfMarkovitz(B,A,0.05)<<endl;
	cout<<returnOfMarkovitz(B,A,0.05)<<endl;
	cout<<riskOfMarkovitz(B,A,0.05)<<endl;
	
	srand(time(NULL)); 
	float a[5][3];
	for(int i=0;i<5;i++)
	{
		a[i][0]=i+1;
		a[i][1]=float(rand()%100); 
		a[i][2]=1;
	}
	for(int i=0;i<5;i++)
	{
		for(int j=0;j<3;j++)
		{
			cout<<a[i][j]<<" ";
		}
		cout<<endl;
	}
	void fit_least_squares(float array[5][3], int a=4);
	fit_least_squares(a);
}

void testOfFunction(){
	//  test for Class Function
	cout<<MyFunction().solve_newton(4)<<endl;  
	cout<<MyFunction().solve_fixed_point(4)<<endl;  
	cout<<MyFunction().solve_bisection(0,3)<<endl;  
	cout<<MyFunction().solve_secant(4)<<endl;  
	cout<<MyFunction().solve_newton_stabilized(1,3)<<endl;  
	cout<<MyFunction().optimize_bisection(2,5)<<endl;  
    cout<<MyFunction().optimize_newton(3)<<endl;  
	cout<<MyFunction().optimize_secant(3)<<endl;  
	cout<<MyFunction().optimize_newton_stabilized(2,5)<<endl;  
	cout<<MyFunction().optimize_golden_search(2,5)<<endl; 


}

void testOfPartial(){
	//  test for Class MultiVariableFunction
	int n=2;
	double a[3]={2,3,5};
	int s = sizeof(a)/sizeof(double);
	cout<<MyOtherFunction().partial(n,s,a)<<endl;
	MyOtherFunction().gradient(s,a);
}

void main()
{
	testOfMatrix();
	testOfFunction();
	testOfPartial();
	
	system("pause");
}