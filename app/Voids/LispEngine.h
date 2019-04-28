#pragma once

//#define ECL_BUILD 0

#ifdef ECL_BUILD

namespace ECL
{
    #include <iostream>
    #include <sstream>

    class LispEngine
    {
    public:
        LispEngine(const char* filePath);

        void Start(void);
        std::string getOutput(void);

       private:
        std::string configurationFile;
        std::string output;
        std::stringstream ss;
    };
};

#endif
