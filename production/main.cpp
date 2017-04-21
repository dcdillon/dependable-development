#include "predictor.hpp"

int main(int argc, char *argv[])
{
    const std::vector< std::string > symbols {"SPY", "XLB", "XLE", "XLF", "XLI",
        "XLK", "XLP", "XLU", "XLV", "XLY", "XME"};
    const std::string date = "2017-01-03";
    
    double betas[11] = {-0.3011671973112251544, -0.0015505374240656672,
        0.1583297052708066144, 0.1186776199122092090, 0.2926841040343490241,
        0.1496979859345836938, -0.5244559054532954567, 0.2096527161095741720,
        0.0767429708349566392, 0.1425406631949422132, -0.1592688290777473648};
        
    double prices[11] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
        
    Reader readers[11];
    
    for (int i = 0; i < 11; ++i)
    {
        readers[i].init(symbols[i], date);
        prices[i] = readers[i].price();
    }
    
    std::string time = readers[0].time();
    
    Predictor predictor(betas, 11, .05);
    
    
    while (readers[0].good())
    {
        std::string time = readers[0].time();
        double price = readers[0].price();
        
        for (int i = 0; i < 11; ++i)
        {
            while (readers[i].time() <= time)
            {
                readers[i].accept();
                if (readers[i].peek_next() == "")
                {
                    break;
                }
            }
            
            prices[i] = readers[i].price();
        }
        
        predictor.update_prices(prices);
        double prediction = predictor.predict(price);
        
        std::cout << time << "," << prediction << std::endl;
        
        readers[0].accept();
    }
    
    return 0;
}