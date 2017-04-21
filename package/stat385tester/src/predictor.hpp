#include <string>
#include <sstream>
#include <fstream>
#include <vector>
#include <cstring>
#include <iostream>

#include <boost/algorithm/string.hpp>

class Predictor
{
public:
    Predictor(double *betas, int count, double alpha)
        : count_(count)
        , alpha_(alpha)
    {
        betas_ = new double[count_];
        emas_ = new double[count_];
        prices_ = new double[count_];
        
        for (int i = 0; i < count_; ++i)
        {
            betas_[i] = betas[i];
            emas_[i] = 0.0;
            prices_[i] = 0.0;
        }
    }
    
    void update_prices(double *prices)
    {
        for (int i = 0; i < count_; ++i)
        {
            prices_[i] = prices[i];
            
            if (emas_[i] == 0)
            {
                emas_[i] = prices_[i];
            }
            else
            {
                emas_[i] = (1 - alpha_) * emas_[i] + alpha_ * prices_[i];
            }
        }
    }
    
    double predict(double current_price)
    {
        double forecast = 0;
        
        for (int i = 0; i < count_; ++i)
        {
            forecast += betas_[i] * (prices_[i] - emas_[i]);
        }
        
        return current_price + forecast;
    }
    
private:
    double *betas_;
    double *emas_;
    double *prices_;
    int count_;
    double alpha_;
};

class Reader
{
public:
    bool init(const std::string &symbol, const std::string &date)
    {
        std::ostringstream buf;
        buf << "../data/" << symbol << "/" << date << ".csv";
        infile_.open(buf.str().c_str());
        // remove the header
        std::string line;
        std::getline(infile_, line);
        
        peek_next();
        accept();
    }
    
    const std::string peek_next()
    {
        do
        {
            if (infile_.good())
            {
                std::string line;
                std::getline(infile_, line);
                boost::algorithm::split(elem_, line, boost::is_any_of(","));
            }
            else
            {
                return "";
            }
        } while (infile_.good() && elem_.size() != 8);
        
        return elem_[0];
    }
    
    void accept()
    {
        if (elem_.size() == 8)
        {
            price_ = atof(elem_[4].c_str());
        }
    }
    
    double price() { return price_; }
    bool good() { return infile_.good(); }
    const std::string &time() { return elem_[0]; }
    
private:
    std::vector< std::string > elem_;
    std::ifstream infile_;
    double price_;
};