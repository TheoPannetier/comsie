// [[Rcpp::plugins(openmp)]]
// [[Rcpp::plugins(cpp14)]]
// [[Rcpp::depends(RcppXsimd)]]

#include <Rcpp.h>
#include <functional>
#include <map>
#include "xsimd/xsimd.hpp"


namespace {

  using simd_type = xsimd::simd_type<float>;
  using simd_vector = std::vector<float, xsimd::aligned_allocator<float, XSIMD_DEFAULT_ALIGNMENT>>;


  // original
  simd_vector get_n_eff(const simd_vector& z, float denom)
  {
    simd_vector n_eff(z.size(), 0.f);
    int z_length = static_cast<int>(z.size());
    for (int i = 0; i < z_length; ++i) {
      float z_i = z[i];
      for (int j = i; j < z_length; ++j) {
        float comp_coeff = expf(-((z_i - z[j]) * (z_i - z[j])) * denom);
        n_eff[i] += comp_coeff;
        if (i != j) n_eff[j] += comp_coeff;
      }
    }
    return n_eff;
  }


  struct reduction_op
  {
    const float zi_;
    const float denom_;

    reduction_op(float zi, float denom) noexcept
      : zi_(zi), denom_(denom)
    {}

    float operator()(float sum, float zj) const noexcept
    {
      return sum + expf(-((zi_ - zj) * (zi_ - zj)) * denom_);
    }
  };


  simd_vector get_n_eff_algo(const simd_vector& z, float denom)
  {
    simd_vector n_eff(z.size(), 0.f);
    for (auto i = 0; i < z.size(); ++i) {
      n_eff[i] = std::accumulate(z.begin(), z.end(), 0.0f, reduction_op(z[i], denom));
    }
    return n_eff;
  }


  simd_vector get_n_eff_omp(const simd_vector& z, float denom)
  {
    simd_vector n_eff(z.size(), 0.f);
    const int N = static_cast<int>(z.size());
#   pragma omp parallel for
    for (int i = 0; i < N; ++i) {
      n_eff[i] = std::accumulate(z.begin(), z.end(), 0.0f, reduction_op(z[i], denom));
    }
    return n_eff;
  }


  simd_vector get_n_eff_simd(const simd_vector& z, float denom)
  {
    const auto N = static_cast<int>(z.size());
    simd_vector n_eff(N, 0.f);
    const auto sdenom = xsimd::set_simd(denom);
    constexpr int ss = static_cast<int>(simd_type::size);
    const int simd_n = N - N % ss;
    for (int i = 0; i < N; ++i) {
      const auto z_i = xsimd::set_simd(z[i]);
      auto ssum = xsimd::zero<simd_type>();
      for (int j = 0; j < simd_n; j += ss) {
        const auto d = z_i - xsimd::load_aligned(&z[j]);
        ssum += xsimd::exp(-(d * d) * sdenom);
      }
      n_eff[i] = std::accumulate(z.begin() + simd_n, z.end(), xsimd::hadd(ssum), reduction_op(z[i], denom));
    }
    return n_eff;
  }


  simd_vector get_n_eff_simd_omp(const simd_vector& z, float denom)
  {
    const auto N = static_cast<int>(z.size());
    simd_vector n_eff(N, 0.f);
    const auto sdenom = xsimd::set_simd(denom);
    constexpr int ss = static_cast<int>(simd_type::size);
    const int simd_n = N - N % ss;
#   pragma omp parallel for
    for (int i = 0; i < N; ++i) {
      const auto z_i = xsimd::set_simd(z[i]);
      auto ssum = xsimd::zero<simd_type>();
      for (int j = 0; j < simd_n; j += ss) {
        const auto d = z_i - xsimd::load_aligned(&z[j]);
        ssum += xsimd::exp(-(d * d) * sdenom);
      }
      n_eff[i] = std::accumulate(z.begin() + simd_n, z.end(), xsimd::hadd(ssum), reduction_op(z[i], denom));
    }
    return n_eff;
  }


  const std::map<std::string, std::function<simd_vector(const simd_vector&, float)>> brute_force_map = {
    {"none", &get_n_eff},
    {"algo", &get_n_eff_algo},
    {"omp", &get_n_eff_omp},
    {"simd", &get_n_eff_simd},
    {"simd_omp", &get_n_eff_simd_omp}
  };

}


using namespace Rcpp;

//' Compute the effective population size
//'
//' Computes \code{n_eff}, the effective population size experienced by an
//' individual.
//' @param z numeric vector, the trait values of all individuals in the
//' community.
//' @param competition_sd numeric `>= 0`. Width of the competition kernel.
//' @param brute_force_opt a string specifying which brute force option to use
//' to speed up the calculation of competition coefficients. Defaults to "none".
//' Other options are omp", for multithreading with OpenMP, "simd" for single
//' instruction, multiple data (SIMD) via the C++ library
//' [`xsimd`](https://github.com/xtensor-stack/xsimd); and "simd_omp" for both.
//' @details `n_eff` sums the competitive effects an individual receives from
//' every individual in the community, including the individual itself. It is
//' called effective population size because it is the size of the population
//' that is relevant for competition.
//' @name get_n_eff_cpp
//' @author Hanno Hildenbrandt
//' @export
// [[Rcpp::export]]
DoubleVector get_n_eff_cpp(const DoubleVector& z, float competition_sd, const std::string& brute_force_opt = "none")
{
  auto it = brute_force_map.find(brute_force_opt);
  if (it == brute_force_map.end()) {
    throw std::runtime_error("invalid argument 'brute_force_opt'");
  }
  simd_vector sz(z.size());
  std::transform(z.begin(), z.end(), sz.begin(), [](double x) {
    return static_cast<float>(x);
  });
  const float denom = 1.0f / (2.f * (competition_sd * competition_sd));
  auto n_eff = it->second(sz, denom);
  auto res = DoubleVector(n_eff.size());
  std::transform(n_eff.cbegin(), n_eff.cend(), res.begin(), [](float x) {
    return static_cast<double>(x);
  });
  return res;
}

//' SIMD size
//'
//' Returns the number of cores that can be used for SIMD
//' @name simd_size
//' @author Hanno Hildenbrandt
//' @export

// [[Rcpp::export]]
int simd_size()
{
  return static_cast<int>(simd_type::size);
}
