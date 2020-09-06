#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   htt\p://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat expit(arma::mat x){
	return 1/(1+exp(-x));
	}

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]

arma::mat score(arma::mat theta, arma::mat X, arma::mat age, arma::mat gender,  arma::mat Y){
  int theta_row = theta.n_rows;
  int theta_col = theta.n_cols;
  
  arma::mat Theta(theta_row*theta_col,theta_col,fill::zeros);
  arma::mat Z;

  for (int i = 0; i < Y.n_cols; i++){
	arma::mat intercept(gender.n_rows,1,fill::ones);
	arma::mat age_dummy_1(age.n_rows,1,fill::zeros);
	age_dummy_1.elem(find(age.col(i)==2)).ones();
	arma::mat age_dummy_2(age.n_rows,1,fill::zeros);
	age_dummy_2.elem(find(age.col(i)==3)).ones();
 
	arma::mat temp = join_rows(join_rows(join_rows(intercept,gender),age_dummy_1),age_dummy_2);

        Z = join_rows(Z,temp);

	Theta(span(i*theta_row,(i+1)*theta_row-1),span(i,i))=theta.col(i);
	}
  
  arma::mat score_mat(X.n_cols,Y.n_cols);

  for (int j = 0; j < X.n_cols; j++){      
        score_mat.row(j)=sum(diagmat(X.col(j))*(Y-expit(Z*Theta)),0);
	}
  return score_mat;
}

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]

arma::mat score_square_X(arma::mat theta, arma::mat X, arma::mat age, arma::mat gender,  arma::mat Y){
  int theta_row = theta.n_rows;
  int theta_col = theta.n_cols;

  arma::mat Theta(theta_row*theta_col,theta_col,fill::zeros);
  arma::mat Z;	

  for (int i = 0; i < Y.n_cols; i++){
        arma::mat intercept(gender.n_rows,1,fill::ones);
        arma::mat age_dummy_1(age.n_rows,1,fill::zeros);
        age_dummy_1.elem(find(age.col(i)==2)).ones();
        arma::mat age_dummy_2(age.n_rows,1,fill::zeros);
        age_dummy_2.elem(find(age.col(i)==3)).ones();

        arma::mat temp = join_rows(join_rows(join_rows(intercept,gender),age_dummy_1),age_dummy_2);

        Z = join_rows(Z,temp);

        Theta(span(i*theta_row,(i+1)*theta_row-1),span(i,i))=theta.col(i);
        }


  arma::mat score_square_x_mat(X.n_cols,Y.n_cols*Y.n_cols);

  for (int j = 0; j < X.n_cols; j++){
        score_square_x_mat.row(j)=reshape(trans(diagmat(X.col(j))*(Y-expit(Z*Theta)))*diagmat(X.col(j))*(Y-expit(Z*Theta)),1,Y.n_cols*Y.n_cols);
        }
  return score_square_x_mat;

	}

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]


arma::mat score_square_Z(arma::mat theta, arma::mat age, arma::mat gender,  arma::mat Y){
  int theta_row = theta.n_rows;
  int theta_col = theta.n_cols;
  int n = Y.n_rows;
      
  arma::mat Theta(theta_row*theta_col,theta_col,fill::zeros);
  arma::mat Z1;
  arma::mat Z2;

  for (int i = 0; i < Y.n_cols; i++){
        arma::mat intercept(gender.n_rows,1,fill::ones);
        arma::mat age_dummy_1(age.n_rows,1,fill::zeros);
        age_dummy_1.elem(find(age.col(i)==2)).ones();
        arma::mat age_dummy_2(age.n_rows,1,fill::zeros);
        age_dummy_2.elem(find(age.col(i)==3)).ones();

        arma::mat temp = join_rows(join_rows(join_rows(intercept,gender),age_dummy_1),age_dummy_2);
        Z1 = join_rows(Z1,temp);
	
        arma::mat temp2 = join_rows(join_rows(join_rows(intercept,gender),age_dummy_1),age_dummy_2);
        Z2 = join_cols(Z2,temp2);
        
	Theta(span(i*theta_row,(i+1)*theta_row-1),span(i,i))=theta.col(i);
        }

  arma::mat Y_all = reshape(Y-expit(Z1*Theta),n*Y.n_cols,1);
  arma::mat S = diagmat(Y_all)*Z2;
  arma::mat SS;
	
  for (int j = 0; j < Y.n_cols; j++){
    
	arma::mat temp = S.rows(j*n,(j+1)*n-1);
 	SS = join_rows(SS,temp);

	}	
   
  arma::mat ss_z = trans(SS)*SS;	
	
  return ss_z;
        
	}

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]


arma::mat score_square_XZ(arma::mat theta, arma::mat X, arma::mat age, arma::mat gender,  arma::mat Y){
  int theta_row = theta.n_rows;
  int theta_col = theta.n_cols;
  int n = Y.n_rows;

  arma::mat Theta(theta_row*theta_col,theta_col,fill::zeros);
  arma::mat Z1;
  arma::mat Z2;

  for (int i = 0; i < Y.n_cols; i++){
        arma::mat intercept(gender.n_rows,1,fill::ones);
        arma::mat age_dummy_1(age.n_rows,1,fill::zeros);
        age_dummy_1.elem(find(age.col(i)==2)).ones();
        arma::mat age_dummy_2(age.n_rows,1,fill::zeros);
        age_dummy_2.elem(find(age.col(i)==3)).ones();

        arma::mat temp = join_rows(join_rows(join_rows(intercept,gender),age_dummy_1),age_dummy_2);
        Z1 = join_rows(Z1,temp);

        arma::mat temp2 = join_rows(join_rows(join_rows(intercept,gender),age_dummy_1),age_dummy_2);
        Z2 = join_cols(Z2,temp2);

        Theta(span(i*theta_row,(i+1)*theta_row-1),span(i,i))=theta.col(i);
        }

  arma::mat Y_all = reshape(Y-expit(Z1*Theta),n*Y.n_cols,1);
  arma::mat S = diagmat(Y_all)*Z2;
  arma::mat SZ;

  for (int j = 0; j < Y.n_cols; j++){

        arma::mat temp = S.rows(j*n,(j+1)*n-1);
        SZ = join_rows(SZ,temp);
        
        }
   

  arma::mat score_square_xz_mat(X.n_cols,Y.n_cols*theta_row*theta_col);

  for (int k = 0; k < X.n_cols; k++){
	arma::mat SX = diagmat(X.col(k))*(Y-expit(Z1*Theta));	
        score_square_xz_mat.row(k)=reshape(trans(SZ)*SX,1,Y.n_cols*theta_row*theta_col);
        }

  return score_square_xz_mat;      
        }



// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]

arma::mat hessian_ZZ(arma::mat theta,  arma::mat age, arma::mat gender){
  int theta_row = theta.n_rows;
  int theta_col = theta.n_cols;
 

  arma::mat Theta(theta_row*theta_col,theta_col,fill::zeros);
  arma::mat Z1;


  for (int i = 0; i < theta_col; i++){
        arma::mat intercept(gender.n_rows,1,fill::ones);
        arma::mat age_dummy_1(age.n_rows,1,fill::zeros);
        age_dummy_1.elem(find(age.col(i)==2)).ones();
        arma::mat age_dummy_2(age.n_rows,1,fill::zeros);
        age_dummy_2.elem(find(age.col(i)==3)).ones();

        arma::mat temp = join_rows(join_rows(join_rows(intercept,gender),age_dummy_1),age_dummy_2);
        Z1 = join_rows(Z1,temp); 

        Theta(span(i*theta_row,(i+1)*theta_row-1),span(i,i))=theta.col(i);
        }
	
  arma::mat H(theta_row*theta_col,theta_row*theta_col,fill::zeros);
  arma::mat PP = (1-expit(Z1*Theta))%expit(Z1*Theta);
 
  for (int j = 0; j < theta_col; j++){

       H(span(j*theta_row,(j+1)*theta_row-1),span(j*theta_row,(j+1)*theta_row-1))=trans(Z1.cols(j*theta_row,(j+1)*theta_row-1))*diagmat(PP.col(j))*Z1.cols(j*theta_row,(j+1)*theta_row-1);

        } 
  
 return H;

        }


// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]


arma::mat hessian_XZ(arma::mat theta, arma::mat X, arma::mat age, arma::mat gender){
  int theta_row = theta.n_rows;
  int theta_col = theta.n_cols;


  arma::mat Theta(theta_row*theta_col,theta_col,fill::zeros);
  arma::mat Z1;


  for (int i = 0; i < theta_col; i++){
        arma::mat intercept(gender.n_rows,1,fill::ones);
        arma::mat age_dummy_1(age.n_rows,1,fill::zeros);
        age_dummy_1.elem(find(age.col(i)==2)).ones();
        arma::mat age_dummy_2(age.n_rows,1,fill::zeros);
        age_dummy_2.elem(find(age.col(i)==3)).ones();

        arma::mat temp = join_rows(join_rows(join_rows(intercept,gender),age_dummy_1),age_dummy_2);
        Z1 = join_rows(Z1,temp);

        Theta(span(i*theta_row,(i+1)*theta_row-1),span(i,i))=theta.col(i);
        }

  arma::mat hessian_xz_mat(X.n_cols,theta_col*theta_row*theta_col);
  arma::mat PP = (1-expit(Z1*Theta))%expit(Z1*Theta);
  
  for (int k = 0; k < X.n_cols; k++){
       
        arma::mat H(theta_col,theta_row*theta_col,fill::zeros);

	for (int j = 0; j < theta_col; j++){
     
 	 H(j,span(j*theta_row,(j+1)*theta_row-1))=trans(X.col(k))*diagmat(PP.col(j))*Z1.cols(j*theta_row,(j+1)*theta_row-1);
         hessian_xz_mat.row(k)=reshape(H,1,theta_col*theta_row*theta_col);

			}

       		 }
	return hessian_xz_mat;
        }

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]

arma::mat test_function(arma::mat s, arma::mat ss_x, arma::mat ss_z, arma::mat ss_xz, arma::mat h_zz,arma::mat h_xz, double N){
  int p = s.n_rows;
  int q = s.n_cols;
  int pq = ss_z.n_cols;

  arma::mat test_stat (p,1);

  for (int i = 0; i < p; i++){
        arma::mat s_temp = s.row(i);
	if (accu(s_temp) != 0){
	arma::mat ss_x_temp = reshape(ss_x.row(i),q,q);
        arma::mat ss_xz_temp = reshape(ss_xz.row(i),pq,q);
	arma::mat h_xz_temp = reshape(h_xz.row(i),q,pq);
 
	arma::mat V = ss_x_temp - 2*h_xz_temp*inv(h_zz)*ss_xz_temp + h_xz_temp*inv(h_zz)*ss_z*inv(h_zz)*trans(h_xz_temp);
	test_stat.row(i) = N*s_temp*inv(V)*trans(s_temp);
	}
	else {test_stat.row(i) = 0;}
	}
  return test_stat;
}	

