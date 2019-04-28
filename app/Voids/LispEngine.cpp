#include "LispEngine.h"

#ifdef ECL_BUILD

    #include "ecl/ecl.h"

    /*************************************** Definition ********************************/
    #define DEFUN(name,fun,args) \
    cl_def_c_function(c_string_to_object(name), \
    (cl_objectfn_fixed)fun, \
    args)


    namespace ECL
    {
        cl_object ecl_call(char *call);

        cl_object ecl_call(char *call)
        {
            return cl_safe_eval(c_string_to_object(call), Cnil, Cnil);
        }

        LispEngine::LispEngine(const char* filePath)
        {
            configurationFile = filePath;

            // redirect console output
            auto old_buf = std::cout.rdbuf(ss.rdbuf());
            std::cout.rdbuf(old_buf);
            output = ss.str();

            // init ecl
            cl_boot(0, (char **)&"");
            atexit(cl_shutdown);

            cl_object ret_load = ecl_call((char*)"(load \"~/quicklisp/setup.lisp\")");
            ret_load = ecl_call((char*)"(princ *default-pathname-defaults*)");
            ret_load = ecl_call((char*)"(ql:quickload :trivial-shell)");
            ret_load = ecl_call((char*)"(ql:quickload :async-syntax)");
            ret_load = ecl_call((char*)"(ql:quickload :marshal)");
            ret_load = ecl_call((char*)"(ql:quickload :lparallel)");
            ret_load = ecl_call((char*)"(ql:quickload :fpp)");
            ret_load = ecl_call((char*)"(ql:quickload :flood)");
            ret_load = ecl_call((char*)"(ql:quickload :ga)");
            ret_load = ecl_call((char*)"(ql:quickload :bordeaux-threads)");
            ret_load = ecl_call((char*)"(asdf:load-system :voids-simulation)");
            //ret_load = ecl_call((char*)"(chdir (concatenate 'string (format nil \"~A\" *default-pathname-defaults*) \"Voids.app/Contents/MacOS/\"))");
            ret_load = ecl_call((char*)"(cd (merge-pathnames (format nil \"~A\" *default-pathname-defaults*) \"Voids.app/Contents/MacOS/\")))");
            //ret_load = ecl_call((char*)"(change-directory (concatenate 'string (format nil \"~A\" *default-pathname-defaults*) \"Voids.app/Contents/MacOS/\"))");
            ret_load = ecl_call((char*)"(setf *default-pathname-defaults* (make-pathname :name (format nil \"~A~A\" *default-pathname-defaults* \"Voids.app/Contents/MacOS/\")))");
            ret_load = ecl_call((char*)"(princ *default-pathname-defaults*)");
        }

        void LispEngine::Start(void)
        {
            // redirect console output
            auto old_buf = std::cout.rdbuf(ss.rdbuf());
            std::cout.rdbuf(old_buf);
            output += ss.str();

            std::string command = "(run \"" + configurationFile + "\")";
            ecl_call((char*)command.c_str());
        }

        std::string LispEngine::getOutput(void)
        {
            return output.c_str();
        }


    };

#endif
