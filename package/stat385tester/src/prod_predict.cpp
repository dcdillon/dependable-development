#include <Rcpp.h>
#include <vector>
#include "predictor.hpp"

// [[Rcpp::export]]
Rcpp::NumericVector prod_predict(Rcpp::NumericMatrix prices)
{
  std::vector< double > predictions;
  double betas[11] = {-0.3011671973112251544, -0.0015505374240656672,
                      0.1583297052708066144, 0.1186776199122092090, 0.2926841040343490241,
                      0.1496979859345836938, -0.5244559054532954567, 0.2096527161095741720,
                      0.0767429708349566392, 0.1425406631949422132, -0.1592688290777473648};

  Predictor predictor(betas, 11, .05);

  for (int i = 0; i < prices.nrow(); ++i)
  {
    double cprices[11];

    for (int j = 0; j < 11; ++j)
    {
      cprices[j] = prices(i, j);
    }

    predictor.update_prices(cprices);
    predictions.push_back(predictor.predict(cprices[0]));
  }

  return Rcpp::wrap(predictions);
}
