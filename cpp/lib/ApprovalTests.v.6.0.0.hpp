// Approval Tests version v.6.0.0
// More information at: https://github.com/approvals/ApprovalTests.cpp
#include <string>
#include <fstream>
#include <stdexcept>
#include <utility>
#include <sys/stat.h>
#include <algorithm>
#include <sstream>
#include <iostream>
#include <stack>
#include <vector>
#include <functional>
#include <tuple>
#include <type_traits>
#include <cstdlib>
#include <numeric>
#include <memory>
#include <exception>
#include <map>
 // ******************** From: Blocker.h
#ifndef APPROVALTESTS_CPP_BLOCKER_H
#define APPROVALTESTS_CPP_BLOCKER_H

namespace ApprovalTests {
class Blocker
{
public:
    virtual ~Blocker() = default;
    virtual bool isBlockingOnThisMachine() const = 0;
};
}

#endif 

 // ******************** From: Macros.h
#ifndef APPROVALTESTS_CPP_MACROS_H
#define APPROVALTESTS_CPP_MACROS_H




#define APPROVAL_TESTS_UNUSED(expr) do { (void)(expr); } while (0)

#if __cplusplus >= 201703L
    #define APPROVAL_TESTS_NO_DISCARD [[nodiscard]]
#else
    #define APPROVAL_TESTS_NO_DISCARD
#endif

#endif 

 // ******************** From: WinMinGWUtils.h
#ifndef APPROVALTESTS_CPP_WINMINGWUTILS_H
#define APPROVALTESTS_CPP_WINMINGWUTILS_H

// <SingleHpp unalterable>

#if (defined(__MINGW32__) || defined(__MINGW64__))
#define APPROVAL_TESTS_MINGW
#endif

#ifdef APPROVAL_TESTS_MINGW
#ifdef __cplusplus
extern "C" {
#endif

#include <sec_api/stdlib_s.h> /* errno_t, size_t */

errno_t getenv_s(
    size_t     *ret_required_buf_size,
    char       *buf,
    size_t      buf_size_in_bytes,
    const char *name
);

#ifdef __cplusplus
}
#endif

#endif // APPROVAL_TESTS_MINGW

// </SingleHpp>

#endif 

 // ******************** From: ApprovalWriter.h
#ifndef APPROVALTESTS_CPP_APPROVALWRITER_H
#define APPROVALTESTS_CPP_APPROVALWRITER_H

namespace ApprovalTests {
class ApprovalWriter
{
public:
    virtual ~ApprovalWriter() = default;
    virtual std::string getFileExtensionWithDot() const = 0;
    virtual void write(std::string path) const = 0;
    virtual void cleanUpReceived(std::string receivedPath) const = 0;
};
}

#endif 

 // ******************** From: StringWriter.h
#ifndef APPROVALTESTS_CPP_STRINGWRITER_H
#define APPROVALTESTS_CPP_STRINGWRITER_H


namespace ApprovalTests {
class StringWriter : public ApprovalWriter
{
private:
    std::string s;
    std::string ext;

public:
    explicit StringWriter( std::string contents, std::string fileExtensionWithDot = ".txt" )
        : s(std::move(contents)), ext(std::move(fileExtensionWithDot)) {}

    std::string getFileExtensionWithDot() const override
    {
        return ext;
    }

    void write( std::string path ) const override
    {
        std::ofstream out( path.c_str(), std::ofstream::out );
        if ( ! out)
        {
            throw std::runtime_error("Unable to write file: " + path);
        }
        this->Write( out );
        out.close();
    }

    void Write( std::ostream &out ) const
    {
        out << s << "\n";
    }

    virtual void cleanUpReceived(std::string receivedPath) const override {
        remove(receivedPath.c_str());
    }


};
}
#endif

 // ******************** From: FileUtils.h




#ifndef APPROVALTESTS_CPP_FILEUTILS_H
#define APPROVALTESTS_CPP_FILEUTILS_H


namespace ApprovalTests {
class FileUtils {
public:
    static bool fileExists(const std::string& path)
    {
        struct stat info{};
        return stat( path.c_str(), &info ) == 0;
    }

    static int fileSize(const std::string& path) {
        struct stat statbuf{};
        int stat_ok = stat(path.c_str(), &statbuf);

        if (stat_ok == -1) {
            return -1;
        }

        return int(statbuf.st_size);
    }

    static void ensureFileExists(const std::string& fullFilePath) {
        if (!fileExists(fullFilePath)) {
            StringWriter s("", "");
            s.write(fullFilePath);
        }
    }

    static std::string getExtensionWithDot(const std::string& filePath) {
        std::size_t found = filePath.find_last_of('.');
        return filePath.substr(found);
    }

    static void writeToFile(const std::string& filePath, const std::string& content)
    {
        std::ofstream out(filePath.c_str(), std::ios::binary | std::ofstream::out);
        if ( ! out)
        {
            throw std::runtime_error("Unable to write file: " + filePath);
        }
        out << content;
    }
};
}

#endif 

 // ******************** From: StringUtils.h


#ifndef APPROVALTESTS_CPP_STRINGUTILS_H
#define APPROVALTESTS_CPP_STRINGUTILS_H


namespace ApprovalTests {
class StringUtils
{
public:
    static std::string replaceAll(std::string inText, const std::string& find, const std::string& replaceWith) {
        size_t start_pos = 0;
        while ((start_pos = inText.find(find, start_pos)) != std::string::npos) {
            inText.replace(start_pos, find.length(), replaceWith);
            start_pos += replaceWith.length(); 
        }
        return inText;
    }

    static bool contains(const std::string& inText, const std::string& find)
    {
        return inText.find(find, 0) != std::string::npos;
    }

    static std::string toLower(std::string inText)
    {
        std::string copy(inText);
        std::transform(inText.begin(), inText.end(), copy.begin(),
          [](char c){ return static_cast<char>(tolower(c)); });
        return copy;
    }

    static bool endsWith(std::string value, std::string ending)
    {
        if (ending.size() > value.size())
        {
            return false;
        }
        return std::equal(ending.rbegin(), ending.rend(), value.rbegin());
    }

    template<typename T>
    static std::string toString(const T& contents)
    {
        std::stringstream s;
        s << contents;
        return s.str();
    }

};
}
#endif 

 // ******************** From: SystemUtils.h
#ifndef APPROVALTESTS_CPP_SYSTEMUTILS_H
#define APPROVALTESTS_CPP_SYSTEMUTILS_H

// <SingleHpp unalterable>
#ifdef _WIN32
    // ReSharper disable once CppUnusedIncludeDirective
    #include <io.h>
    #include <windows.h>
    #include <direct.h>
#else
    // ReSharper disable once CppUnusedIncludeDirective
    #include <unistd.h>
#endif
// </SingleHpp>



namespace ApprovalTests {
class SystemUtils
{
public:
    static bool isWindowsOs()
    {
#ifdef _WIN32
        return true;
#else
        return false;
#endif

    }

    static bool isCygwin()
    {
#ifdef __CYGWIN__
        return true;
#else
        return false;
#endif
    }

    static std::string getDirectorySeparator()
    {
        return isWindowsOs() ? "\\" : "/";
    }

    
    static std::string checkFilenameCase(const std::string& fullPath)
    {
        if (!isWindowsOs() || !FileUtils::fileExists(fullPath))
        {
            return fullPath;
        }
#ifdef _WIN32

        WIN32_FIND_DATAA findFileData;
        HANDLE hFind = FindFirstFileA(fullPath.c_str(), &findFileData);

        if (hFind != INVALID_HANDLE_VALUE)
        {
            const std::string fixedFilename = findFileData.cFileName;
            const std::string fixedPath =
                StringUtils::replaceAll( fullPath, StringUtils::toLower(fixedFilename), fixedFilename );
            FindClose(hFind);
            return fixedPath;
        }


#endif
        return fullPath;

    }

    static std::string safeGetEnvForWindows(char const *name)
    {
        APPROVAL_TESTS_UNUSED(name);
#ifdef _WIN32
        
        
        

        size_t size;
        getenv_s(&size, nullptr, 0, name);

        if (size != 0)
        {
            std::string result;
            result.resize(size);
            getenv_s(&size, &*result.begin(), size, name);
            result.pop_back();
            return result;
        }
#endif
        return std::string();
    }

    static std::string safeGetEnvForNonWindows(char const *name)
    {
        APPROVAL_TESTS_UNUSED(name);
        char* p = nullptr;
#ifndef _WIN32
        p = getenv(name);
#endif
        return (p != nullptr) ? p : std::string();
    }

    
    static std::string safeGetEnv(char const *name)
    {
        return isWindowsOs() ? safeGetEnvForWindows(name) : safeGetEnvForNonWindows(name);
    }

    static std::string getMachineName()
    {
        auto name = safeGetEnv("COMPUTERNAME");
        if ( ! name.empty())
        {
            return name;
        }

        name = safeGetEnv("HOSTNAME");
        if ( ! name.empty())
        {
            return name;
        }

        return "Unknown Computer";
    }

    static void makeDirectoryForWindows(const std::string& directory)
    {
        APPROVAL_TESTS_UNUSED(directory);
#ifdef _WIN32
        int nError = _mkdir(directory.c_str());
        if (nError != 0)
        {
            std::string helpMessage = std::string("Unable to create directory: ") + directory;
            throw std::runtime_error( helpMessage );
        }
#endif
    }

    static void makeDirectoryForNonWindows(const std::string& directory)
    {
        APPROVAL_TESTS_UNUSED(directory);
#ifndef _WIN32
        mode_t nMode = 0733; 
        int nError = mkdir(directory.c_str(),nMode);
        if (nError != 0)
        {
            std::string helpMessage = std::string("Unable to create directory: ") + directory;
            throw std::runtime_error( helpMessage );
        }
#endif
    }

    static void makeDirectory(const std::string& directory)
    {
        makeDirectoryForWindows(directory);
        makeDirectoryForNonWindows(directory);
    }

    static void ensureDirectoryExists(const std::string& fullFilePath)
    {
        if (!FileUtils::fileExists(fullFilePath))
        {
            makeDirectory(fullFilePath);
        }
    }
};
}
#endif

 // ******************** From: MachineBlocker.h
#ifndef APPROVALTESTS_CPP_MACHINEBLOCKER_H
#define APPROVALTESTS_CPP_MACHINEBLOCKER_H


namespace ApprovalTests {
class MachineBlocker : public Blocker
{
private:
    std::string machineName;
    bool block;

    MachineBlocker() = delete;

public:
    MachineBlocker(std::string machineName, bool block ) : machineName(std::move(machineName)), block(block)
    {
    }

    static MachineBlocker onMachineNamed( const std::string& machineName )
    {
        return MachineBlocker(machineName, true);
    }

    static MachineBlocker onMachinesNotNamed( const std::string& machineName )
    {
        return MachineBlocker(machineName, false);
    }

    virtual bool isBlockingOnThisMachine() const override
    {
        const auto isMachine = (SystemUtils::getMachineName() == machineName);
        return isMachine == block;
    }
};
}

#endif 

 // ******************** From: FileUtilsSystemSpecific.h
#ifndef APPROVALTESTS_CPP_FILEUTILSSYSTEMSPECIFIC_H
#define APPROVALTESTS_CPP_FILEUTILSSYSTEMSPECIFIC_H


namespace ApprovalTests {
class FileUtilsSystemSpecific
{
public:
    static std::string getCommandLineForCopy(const std::string& source, const std::string& destination, bool isWindows)
    {
        if (isWindows) {
            return std::string("copy /Y ") + "\"" + source + "\" \"" + destination + "\"";
        } else {
            return std::string("cp ") + "\"" + source + "\" \"" + destination + "\"";
        }
    }

    static void copyFile(const std::string& source, const std::string& destination )
    {
        system( getCommandLineForCopy(source, destination, SystemUtils::isWindowsOs()).c_str() );
    }
};
}
#endif

 // ******************** From: Reporter.h
#ifndef APPROVALTESTS_CPP_REPORTER_H
#define APPROVALTESTS_CPP_REPORTER_H


namespace ApprovalTests {

class Reporter {
public:
    virtual ~Reporter() = default;
    virtual bool report(std::string received, std::string approved) const = 0;
};


template<typename T>
using IsNotDerivedFromReporter = typename std::enable_if<!std::is_base_of<Reporter, T>::value, int>::type;
}

#endif

 // ******************** From: AutoApproveReporter.h
#ifndef APPROVALTESTS_CPP_AUTOAPPROVEREPORTER_H
#define APPROVALTESTS_CPP_AUTOAPPROVEREPORTER_H



namespace ApprovalTests {
class AutoApproveReporter : public Reporter
{
public:
    bool report(std::string received, std::string approved) const override
    {
        std::cout << "file " << approved << " automatically approved - next run should succeed\n";
        FileUtilsSystemSpecific::copyFile( received, approved );
        return true;
    }
};
}

#endif

 // ******************** From: ApprovalNamer.h
#ifndef APPROVALTESTS_CPP_APPROVALNAMER_H
#define APPROVALTESTS_CPP_APPROVALNAMER_H


namespace ApprovalTests {
class ApprovalNamer
{
public:
    virtual ~ApprovalNamer() = default;
    virtual std::string getApprovedFile(std::string extensionWithDot) const = 0;
    virtual std::string getReceivedFile(std::string extensionWithDot) const = 0;

};
}

#endif

 // ******************** From: ApprovalTestNamer.h
#ifndef APPROVALTESTS_CPP_APPROVALTESTNAMER_H
#define APPROVALTESTS_CPP_APPROVALTESTNAMER_H


namespace ApprovalTests {
class TestName {
public:
    const std::string& getFileName() const {
        return fileName;
    }

    void setFileName(const std::string &file) {
        fileName = SystemUtils::checkFilenameCase(file);
    }

    std::vector<std::string> sections;
private:
    std::string fileName;
};

class TestConfiguration {
public:
    std::string subdirectory;
};

class ApprovalTestNamer : public ApprovalNamer {
private:
public:
    ApprovalTestNamer() = default;

    std::string getTestName() const {
        std::stringstream ext;
        auto test = getCurrentTest();
        for (size_t i = 0; i < test.sections.size(); i++) {
            if (0 < i) {
                ext << ".";
            }
            ext << test.sections[i];
        }

        return convertToFileName(ext.str());
    }

    static bool isForbidden(char c)
    {
        static std::string forbiddenChars("\\/:?\"<>|' ");
        return std::string::npos != forbiddenChars.find(c);
    }

    static std::string convertToFileName(const std::string& fileName)
    {
        std::stringstream result;
        for (auto ch : fileName)
        {
            if (!isForbidden(ch))
            {
                result << ch;
            }
            else
            {
                result << "_";
            }
        }
        return result.str();
    }

    static TestName &getCurrentTest()
    {
        try
        {
            return currentTest();
        }
        catch( const std::runtime_error& )
        {
            std::string helpMessage = getMisconfiguredMainHelp();
            throw std::runtime_error( helpMessage );
        }
    }

// <SingleHpp unalterable>
    static std::string getMisconfiguredMainHelp()
    {
        std::string lineBreak = "************************************************************************************n";
        std::string lineBuffer = "*                                                                                  *n";
        std::string helpMessage =
                "nn" + lineBreak + lineBuffer +
R"(* Welcome to Approval Tests.
*
* You have forgotten to configure your test framework for Approval Tests.
*
* To do this in Catch, add the following to your main.cpp:
*
*     #define APPROVALS_CATCH
*     #include "ApprovalTests.hpp"
*
* To do this in Google Test, add the following to your main.cpp:
*
*     #define APPROVALS_GOOGLETEST
*     #include "ApprovalTests.hpp"
*
* To do this in doctest, add the following to your main.cpp:
*
*     #define APPROVALS_DOCTEST
*     #include "ApprovalTests.hpp"
*
* For more information, please visit:
* https://github.com/approvals/ApprovalTests.cpp/blob/master/doc/GettingStarted.md
)" +
                    lineBuffer + lineBreak + 'n';
        return helpMessage;
    }
// </SingleHpp>


    
    std::string getFileName() const {
        return getSourceFileName();
    }


    std::string getSourceFileName() const {
        auto file = getCurrentTest().getFileName();
        auto start = file.rfind(SystemUtils::getDirectorySeparator()) + 1;
        auto end = file.rfind('.');
        auto fileName = file.substr(start, end - start);
        return convertToFileName(fileName);
    }

    std::string getDirectory() const {
        auto file = getCurrentTest().getFileName();
        auto end = file.rfind(SystemUtils::getDirectorySeparator()) + 1;
        auto directory = file.substr(0, end);
        if ( ! testConfiguration().subdirectory.empty() )
        {
            directory += testConfiguration().subdirectory + SystemUtils::getDirectorySeparator();
            SystemUtils::ensureDirectoryExists(directory);
        }
        return directory;
    }

    static TestName& currentTest(TestName* value = nullptr)
    {
        static TestName* staticValue;
        if (value != nullptr)
        {
            staticValue = value;
        }
        if ( staticValue == nullptr )
        {
            throw std::runtime_error("The variable in currentTest() is not initialised");
        }
        return *staticValue;
    }

    static TestConfiguration& testConfiguration()
    {
        static TestConfiguration configuration;
        return configuration;
    }

    virtual std::string getApprovedFile(std::string extensionWithDot) const override {

        return getFullFileName(".approved", extensionWithDot);
    }

    virtual std::string getReceivedFile(std::string extensionWithDot) const override {

        return getFullFileName(".received", extensionWithDot);
    }

    std::string getOutputFileBaseName() const {
        return getSourceFileName() + "." + getTestName();
    }

    std::string getFullFileName(const std::string& approved, const std::string& extensionWithDot) const {
        std::stringstream ext;
        ext << getDirectory() << getOutputFileBaseName() << approved << extensionWithDot;
        return ext.str();
    }
};
}

#endif

 // ******************** From: SectionNameDisposer.h
#ifndef APPROVALTESTS_CPP_SECTIONNAMEDISPOSER_H
#define APPROVALTESTS_CPP_SECTIONNAMEDISPOSER_H


namespace ApprovalTests {
class APPROVAL_TESTS_NO_DISCARD SectionNameDisposer
{
public:
    SectionNameDisposer(TestName& currentTest, const std::string& scope_name) :
        currentTest(currentTest)
    {
        
        
        currentTest.sections.push_back(scope_name);
    }

    ~SectionNameDisposer()
    {
        
        currentTest.sections.pop_back();
    }
private:
    TestName& currentTest;
};
}

#endif 

 // ******************** From: GoogleCustomizationsFactory.h
#ifndef APPROVALTESTS_CPP_GOOGLECUSTOMIZATIONSFACTORY_H
#define APPROVALTESTS_CPP_GOOGLECUSTOMIZATIONSFACTORY_H



namespace ApprovalTests {
class GoogleCustomizationsFactory
{
public:
    using Comparator = std::function<bool(const std::string&, const std::string&)>;
private:
    using ComparatorContainer = std::vector< Comparator >;
    static ComparatorContainer& comparatorContainer()
    {
        static ComparatorContainer container;
        if (container.empty())
        {
            auto exactNameMatching = [](const std::string& testFileNameWithExtension, const std::string& testCaseName)
            {
                return StringUtils::contains(testFileNameWithExtension, testCaseName + ".");
            };
            container.push_back( exactNameMatching );
        }
        return container;
    }

public:
    static ComparatorContainer getEquivalencyChecks()
    {
        return comparatorContainer();
    }

    APPROVAL_TESTS_NO_DISCARD static bool addTestCaseNameRedundancyCheck(const Comparator& comparator)
    {
        comparatorContainer().push_back(comparator);
        return true;
    }
    

};
}

#endif 

 // ******************** From: CartesianProduct.h
#ifndef APPROVALTESTS_CPP_CARTESIANPRODUCT_H
#define APPROVALTESTS_CPP_CARTESIANPRODUCT_H


namespace ApprovalTests {
namespace CartesianProduct {
namespace Detail {



template<bool B, class T=void>
using enable_if_t = typename std::enable_if<B, T>::type;


template<std::size_t... Is>
struct index_sequence {};

template<std::size_t N, std::size_t... Is>
struct make_index_sequence : make_index_sequence<N-1, N-1, Is...> {};

template<std::size_t... Is>
struct make_index_sequence<0, Is...> : index_sequence<Is...> {};




template<class Tuple>
constexpr std::size_t tuple_size() {
    return std::tuple_size<typename std::remove_reference<Tuple>::type>::value;
}

template<class Tuple>
using make_tuple_idxs = make_index_sequence<tuple_size<Tuple>()>;



template <class F, class Tuple, std::size_t... I>
constexpr auto apply_impl(F&& f, Tuple&& t, index_sequence<I...>)
    -> decltype(std::forward<F>(f)(std::get<I>(std::forward<Tuple>(t))...))
{
    return std::forward<F>(f)(std::get<I>(std::forward<Tuple>(t))...);
}

template <class F, class Tuple>
auto apply(F&& f, Tuple&& t)
    -> decltype(apply_impl(std::forward<F>(f), std::forward<Tuple>(t), make_tuple_idxs<Tuple>{}))
{
    apply_impl(std::forward<F>(f), std::forward<Tuple>(t), make_tuple_idxs<Tuple>{});
}


template <class Tuple, class F, std::size_t... Is>
void for_each_impl(Tuple&& t, F&& f, index_sequence<Is...>) {
    (void)std::initializer_list<int>{
        (std::forward<F>(f)(std::get<Is>(std::forward<Tuple>(t))), 0)...
    };
}

template <class Tuple, class F>
void for_each(Tuple&& t, F&& f) {
    for_each_impl(std::forward<Tuple>(t), std::forward<F>(f), make_tuple_idxs<Tuple>{});
}

template<class Tuple, class F, std::size_t... Is>
auto transform_impl(Tuple&& t, F&& f, index_sequence<Is...>)
    -> decltype(std::make_tuple(std::forward<F>(f)(std::get<Is>(std::forward<Tuple>(t)))...))
{
    return std::make_tuple(std::forward<F>(f)(std::get<Is>(std::forward<Tuple>(t)))...);
}

template<class F, class Tuple>
auto transform(Tuple&& t, F&& f = {})
    -> decltype(transform_impl(std::forward<Tuple>(t), std::forward<F>(f), make_tuple_idxs<Tuple>{}))
{
    return transform_impl(std::forward<Tuple>(t), std::forward<F>(f), make_tuple_idxs<Tuple>{});
}

template<class Predicate>
struct find_if_body {
    const Predicate& pred;
    std::size_t& index;
    std::size_t currentIndex = 0;
    bool found = false;

    find_if_body(const Predicate& p, std::size_t& i) : pred(p), index(i) {}

    template<typename T>
    void operator()(T&& value) {
        if (found) return;
        if (pred(std::forward<T>(value))) {
            index = currentIndex;
            found = true;
        }
        ++currentIndex;
    }
};

template<class Predicate, class Tuple>
std::size_t find_if(Tuple&& tuple, Predicate pred = {}) {
    std::size_t idx = tuple_size<Tuple>();
    for_each(std::forward<Tuple>(tuple), find_if_body<Predicate>(pred, idx));
    return idx;
}

template<class Predicate, class Tuple>
bool any_of(Tuple&& tuple, Predicate pred = {}) {
    return find_if(std::forward<Tuple>(tuple), pred) != tuple_size<Tuple>();
}

struct is_range_empty {
    template<class T>
    bool operator()(const T& range) const {
        using std::begin;
        using std::end;
        return begin(range) == end(range);
    }
};


struct dereference_iterator {
    template<class It>
    auto operator()(It&& it) const -> decltype(*std::forward<It>(it)) {
        return *std::forward<It>(it);
    }
};


template<class Its, std::size_t I = tuple_size<Its>()-1>
enable_if_t<I == 0>
increment_iterator(Its& it, const Its&, const Its&) {
    ++std::get<I>(it);
}


template<class Its, std::size_t I = tuple_size<Its>()-1>
enable_if_t<I != 0>
increment_iterator(Its& its, const Its& begins, const Its& ends) {
    if (++std::get<I>(its) == std::get<I>(ends)) {
        std::get<I>(its) = std::get<I>(begins);
        increment_iterator<Its, I-1>(its, begins, ends);
    }
}
} 





template<class F, class... Ranges>
void cartesian_product(F&& f, const Ranges&... ranges) {
    using std::begin;
    using std::end;

    if (Detail::any_of<Detail::is_range_empty>(std::forward_as_tuple(ranges...)))
        return;

    const auto begins = std::make_tuple(begin(ranges)...);
    const auto ends = std::make_tuple(end(ranges)...);

    for (auto its = begins; std::get<0>(its) != std::get<0>(ends); Detail::increment_iterator(its, begins, ends)) {
        
        
        
        
        Detail::apply(std::forward<F>(f), Detail::transform<Detail::dereference_iterator>(its));
    }
}
} 
} 

#endif

 // ******************** From: ExistingFile.h
#ifndef APPROVALTESTS_CPP_EXISTINGFILE_H
#define APPROVALTESTS_CPP_EXISTINGFILE_H



namespace ApprovalTests {
class ExistingFile : public ApprovalWriter{
    std::string filePath;
public:
    explicit ExistingFile(std::string filePath) : filePath(std::move(filePath)){}
    virtual std::string getFileExtensionWithDot() const override {
        return FileUtils::getExtensionWithDot(filePath);
    }
    virtual void write(std::string ) const override {
        
    }
    virtual void cleanUpReceived(std::string ) const override {
        
    }
};
}

#endif

 // ******************** From: CommandLauncher.h
#ifndef APPROVALTESTS_CPP_COMMANDLAUNCHER_H
#define APPROVALTESTS_CPP_COMMANDLAUNCHER_H


namespace ApprovalTests {

class CommandLauncher
{
public:
    virtual ~CommandLauncher() = default;
    virtual bool launch(std::vector<std::string> argv) = 0;
};
}

#endif  

 // ******************** From: SystemLauncher.h

#ifndef APPROVALTESTS_CPP_SYSTEMLAUNCHER_H
#define APPROVALTESTS_CPP_SYSTEMLAUNCHER_H


namespace ApprovalTests {
    using ConvertArgumentsFunctionPointer = std::vector<std::string>(*)(std::vector<std::string>);

class SystemLauncher : public CommandLauncher
{
private:
    ConvertArgumentsFunctionPointer convertArgumentsForSystemLaunching;
public:
    SystemLauncher() : SystemLauncher(doNothing)
    {
    }

    explicit SystemLauncher(std::vector<std::string> (*pointer)(std::vector<std::string>)) : convertArgumentsForSystemLaunching(pointer) 
    {
    }

    
    void setConvertArgumentsForSystemLaunchingFunction(ConvertArgumentsFunctionPointer function)
    {
        convertArgumentsForSystemLaunching = function;
    }

    bool exists(const std::string& command)
    {
        bool foundByWhich = false;
        if (!SystemUtils::isWindowsOs()) {
            std::string which = "which " + command + " > /dev/null 2>&1";
            int result = system(which.c_str());
            foundByWhich = (result == 0);
        }
        return  foundByWhich || FileUtils::fileExists(command);

    }

    static std::vector<std::string> doNothing(std::vector<std::string> argv)
    {
        return argv;
    }

    bool launch(std::vector<std::string> argv) override
    {
        if (!exists(argv.front()))
        {
            return false;
        }

        argv = convertArgumentsForSystemLaunching(argv);

        std::string command = std::accumulate(argv.begin(), argv.end(), std::string(""), [](const std::string& a, const std::string& b) {return a + " " + "\"" + b + "\""; });
        std::string launch = SystemUtils::isWindowsOs() ? ("start \"\" " + command) : (command + " &");
        system(launch.c_str());
        return true;
    }
};
}

#endif 

 // ******************** From: CommandReporter.h
#ifndef APPROVALTESTS_CPP_COMMANDREPORTER_H
#define APPROVALTESTS_CPP_COMMANDREPORTER_H


namespace ApprovalTests {

class CommandReporter : public Reporter {
private:
    std::string cmd;
    CommandLauncher *l;

protected:
    CommandReporter(std::string command, CommandLauncher *launcher)
            : cmd(std::move(command)), l(launcher) {
    }

public:
    bool report(std::string received, std::string approved) const override {
        FileUtils::ensureFileExists(approved);
        return l->launch(getFullCommand(received, approved));
    }

    std::vector<std::string> getFullCommand(const std::string &received, const std::string &approved) const
    {
        std::vector<std::string> fullCommand;
        fullCommand.push_back(cmd);
        fullCommand.push_back(received);
        fullCommand.push_back(approved);
        return fullCommand;
    }
};
}
#endif 

 // ******************** From: DiffInfo.h
#ifndef APPROVALTESTS_CPP_DIFFINFO_H
#define APPROVALTESTS_CPP_DIFFINFO_H


namespace ApprovalTests {
enum class Type { TEXT, IMAGE, TEXT_AND_IMAGE };



struct DiffInfo
{
    DiffInfo(std::string program, Type type) :
        program(std::move(program)),
        arguments("%s %s"),
        type(type)
    {
    }
    DiffInfo(std::string program, std::string arguments, Type type) :
        program(std::move(program)),
        arguments(std::move(arguments)),
        type(type)
    {
    }
    std::string program;
    std::string arguments;
    Type type;

    std::string getProgramForOs() const
    {
        std::string result = program;
        if (result.rfind("{ProgramFiles}", 0) == 0)
        {
            const std::vector<const char*> envVars =
            {
                "ProgramFiles",
                "ProgramW6432",
                "ProgramFiles(x86)"
            };

            for(const auto& envVar : envVars)
            {
                std::string envVarValue = SystemUtils::safeGetEnv(envVar);
                if (envVarValue.empty())
                {
                    continue;
                }
                envVarValue += '\\';

                auto result1 = StringUtils::replaceAll(result, "{ProgramFiles}", envVarValue);
                if (FileUtils::fileExists(result1))
                {
                    return result1;
                }
            }
        }
        return result;
    }
};
}

#endif 

 // ******************** From: DiffPrograms.h
#ifndef APPROVALTESTS_CPP_DIFFPROGRAMS_H
#define APPROVALTESTS_CPP_DIFFPROGRAMS_H



#define APPROVAL_TESTS_MACROS_ENTRY(name, defaultValue) \
        static DiffInfo name() { return defaultValue; }


namespace ApprovalTests {
namespace DiffPrograms {


    namespace Mac {
        APPROVAL_TESTS_MACROS_ENTRY(DIFF_MERGE,
              DiffInfo("/Applications/DiffMerge.app/Contents/MacOS/DiffMerge", "%s %s -nosplash", Type::TEXT))

        APPROVAL_TESTS_MACROS_ENTRY(BEYOND_COMPARE, DiffInfo("/Applications/Beyond Compare.app/Contents/MacOS/bcomp", Type::TEXT))

        APPROVAL_TESTS_MACROS_ENTRY(KALEIDOSCOPE, DiffInfo("/Applications/Kaleidoscope.app/Contents/MacOS/ksdiff", Type::TEXT_AND_IMAGE))

        APPROVAL_TESTS_MACROS_ENTRY(KDIFF3, DiffInfo("/Applications/kdiff3.app/Contents/MacOS/kdiff3", "%s %s -m", Type::TEXT))

        APPROVAL_TESTS_MACROS_ENTRY(P4MERGE, DiffInfo("/Applications/p4merge.app/Contents/MacOS/p4merge", Type::TEXT_AND_IMAGE))

        APPROVAL_TESTS_MACROS_ENTRY(TK_DIFF, DiffInfo("/Applications/TkDiff.app/Contents/MacOS/tkdiff", Type::TEXT))

        APPROVAL_TESTS_MACROS_ENTRY(VS_CODE, DiffInfo("/Applications/Visual Studio Code.app/Contents/Resources/app/bin/code", "-d %s %s", Type::TEXT))
    }
    namespace Linux {
        
        APPROVAL_TESTS_MACROS_ENTRY(KDIFF3, DiffInfo("kdiff3", Type::TEXT))

        APPROVAL_TESTS_MACROS_ENTRY(MELD, DiffInfo("meld", Type::TEXT))
    }
    namespace Windows {
        APPROVAL_TESTS_MACROS_ENTRY(BEYOND_COMPARE_3, DiffInfo("{ProgramFiles}Beyond Compare 3\\BCompare.exe", Type::TEXT_AND_IMAGE))

        APPROVAL_TESTS_MACROS_ENTRY(BEYOND_COMPARE_4, DiffInfo("{ProgramFiles}Beyond Compare 4\\BCompare.exe", Type::TEXT_AND_IMAGE))

        APPROVAL_TESTS_MACROS_ENTRY(TORTOISE_IMAGE_DIFF,
              DiffInfo("{ProgramFiles}TortoiseSVN\\bin\\TortoiseIDiff.exe", "/left:%s /right:%s", Type::IMAGE))

        APPROVAL_TESTS_MACROS_ENTRY(TORTOISE_TEXT_DIFF, DiffInfo("{ProgramFiles}TortoiseSVN\\bin\\TortoiseMerge.exe", Type::TEXT))

        APPROVAL_TESTS_MACROS_ENTRY(WIN_MERGE_REPORTER, DiffInfo("{ProgramFiles}WinMerge\\WinMergeU.exe", Type::TEXT_AND_IMAGE))

        APPROVAL_TESTS_MACROS_ENTRY(ARAXIS_MERGE, DiffInfo("{ProgramFiles}Araxis\\Araxis Merge\\Compare.exe", Type::TEXT_AND_IMAGE))

        APPROVAL_TESTS_MACROS_ENTRY(CODE_COMPARE, DiffInfo("{ProgramFiles}Devart\\Code Compare\\CodeCompare.exe", Type::TEXT))

        APPROVAL_TESTS_MACROS_ENTRY(KDIFF3, DiffInfo("{ProgramFiles}KDiff3\\kdiff3.exe", Type::TEXT))
        APPROVAL_TESTS_MACROS_ENTRY(VS_CODE, DiffInfo("{ProgramFiles}Microsoft VS Code\\Code.exe", "-d %s %s", Type::TEXT))

    }
}
}

#endif 

 // ******************** From: GenericDiffReporter.h
#ifndef APPROVALTESTS_CPP_GENERICDIFFREPORTER_H
#define APPROVALTESTS_CPP_GENERICDIFFREPORTER_H


namespace ApprovalTests {
class GenericDiffReporter : public CommandReporter {
private:
    SystemLauncher launcher;
public:
    explicit GenericDiffReporter(const std::string& program) : CommandReporter(program, &launcher)
    {
        checkForCygwin();
    }
    explicit GenericDiffReporter(const DiffInfo& info) : CommandReporter(info.getProgramForOs(), &launcher)
    {
        checkForCygwin();
    }

    void checkForCygwin()
    {
        if ( SystemUtils::isCygwin())
        {
            launcher.setConvertArgumentsForSystemLaunchingFunction(convertForCygwin);
        }
    }

    static std::vector<std::string> convertForCygwin(std::vector<std::string> argv)
    {
        if (! SystemUtils::isCygwin())
        {
            return argv;
        }
        std::vector<std::string> copy = argv;
        for( size_t i = 0; i != argv.size(); ++i )
        {
            if ( i == 0)
            {
                copy[i] = "$(cygpath '"  + argv[i] + "')";
            }
            else
            {
                copy[i] = "$(cygpath -aw '"  + argv[i] + "')";
            }
        }
        return copy;
    }
};
}

#endif 

 // ******************** From: FirstWorkingReporter.h
#ifndef APPROVALTESTS_CPP_FIRSTWORKINGREPORTER_H
#define APPROVALTESTS_CPP_FIRSTWORKINGREPORTER_H


namespace ApprovalTests {
class FirstWorkingReporter : public Reporter
{
private:
    std::vector< std::unique_ptr<Reporter> > reporters;
public:
    
    explicit FirstWorkingReporter(const std::vector<Reporter*>& theReporters)
    {
        for(auto r : theReporters)
        {
            reporters.push_back(std::unique_ptr<Reporter>(r));
        }
    }

    bool report(std::string received, std::string approved) const override
    {
        for(auto& r : reporters)
        {
            if (r->report(received, approved))
            {
                return true;
            }
        }
        return false;
    }
};
}

#endif 

 // ******************** From: LinuxReporters.h
#ifndef APPROVALTESTS_CPP_LINUXREPORTERS_H
#define APPROVALTESTS_CPP_LINUXREPORTERS_H


namespace ApprovalTests {
namespace Linux
{
    class KDiff3Reporter : public GenericDiffReporter {
    public:
        KDiff3Reporter() : GenericDiffReporter(DiffPrograms::Linux::KDIFF3()) {}
    };

    class MeldReporter : public GenericDiffReporter {
    public:
        MeldReporter() : GenericDiffReporter(DiffPrograms::Linux::MELD()) {}
    };

    class LinuxDiffReporter : public FirstWorkingReporter
    {
    public:
        LinuxDiffReporter() : FirstWorkingReporter(
                {
                        
                        new MeldReporter(),
                        new KDiff3Reporter()
                        
                }
        )
        {
        }
    };

}
}

#endif 

 // ******************** From: MacReporters.h
#ifndef APPROVALTESTS_CPP_MACREPORTERS_H
#define APPROVALTESTS_CPP_MACREPORTERS_H


namespace ApprovalTests {
namespace Mac {
    class DiffMergeReporter : public GenericDiffReporter {
    public:
        DiffMergeReporter() : GenericDiffReporter(DiffPrograms::Mac::DIFF_MERGE()) {}
    };

    class VisualStudioCodeReporter : public GenericDiffReporter {
    public:
        VisualStudioCodeReporter() : GenericDiffReporter(DiffPrograms::Mac::VS_CODE()) {}
    };

    class BeyondCompareReporter : public GenericDiffReporter {
    public:
        BeyondCompareReporter() : GenericDiffReporter(DiffPrograms::Mac::BEYOND_COMPARE()) {}
    };

    class KaleidoscopeReporter : public GenericDiffReporter {
    public:
        KaleidoscopeReporter() : GenericDiffReporter(DiffPrograms::Mac::KALEIDOSCOPE()) {}
    };

    class KDiff3Reporter : public GenericDiffReporter {
    public:
        KDiff3Reporter() : GenericDiffReporter(DiffPrograms::Mac::KDIFF3()) {}
    };

    class P4MergeReporter : public GenericDiffReporter {
    public:
        P4MergeReporter() : GenericDiffReporter(DiffPrograms::Mac::P4MERGE()) {}
    };

    class TkDiffReporter : public GenericDiffReporter {
    public:
        TkDiffReporter() : GenericDiffReporter(DiffPrograms::Mac::TK_DIFF()) {}
    };

    class MacDiffReporter : public FirstWorkingReporter {
    public:
        MacDiffReporter() : FirstWorkingReporter(
                {
                        
                        new BeyondCompareReporter(),
                        new DiffMergeReporter(),
                        new KaleidoscopeReporter(),
                        new P4MergeReporter(),
                        new KDiff3Reporter(),
                        new TkDiffReporter(),
                        new VisualStudioCodeReporter()
                        
                }
        ) {
        }
    };
}
}

#endif 

 // ******************** From: WindowsReporters.h
#ifndef APPROVALTESTS_CPP_WINDOWSREPORTERS_H
#define APPROVALTESTS_CPP_WINDOWSREPORTERS_H


namespace ApprovalTests {
namespace Windows {
    class BeyondCompare3Reporter : public GenericDiffReporter {
    public:
        BeyondCompare3Reporter() : GenericDiffReporter(DiffPrograms::Windows::BEYOND_COMPARE_3()) {}
    };

  class VisualStudioCodeReporter : public GenericDiffReporter {
    public:
      VisualStudioCodeReporter() : GenericDiffReporter(DiffPrograms::Windows::VS_CODE()) {}
    };

    class BeyondCompare4Reporter : public GenericDiffReporter {
    public:
        BeyondCompare4Reporter() : GenericDiffReporter(DiffPrograms::Windows::BEYOND_COMPARE_4()) {}
    };

    class BeyondCompareReporter : public FirstWorkingReporter {
    public:
        BeyondCompareReporter() : FirstWorkingReporter({new BeyondCompare4Reporter(), new BeyondCompare3Reporter()}) {
        }
    };

    class TortoiseImageDiffReporter : public GenericDiffReporter {
    public:
        TortoiseImageDiffReporter() : GenericDiffReporter(DiffPrograms::Windows::TORTOISE_IMAGE_DIFF()) {}
    };

    class TortoiseTextDiffReporter : public GenericDiffReporter {
    public:
        TortoiseTextDiffReporter() : GenericDiffReporter(DiffPrograms::Windows::TORTOISE_TEXT_DIFF()) {}
    };

    class TortoiseDiffReporter : public FirstWorkingReporter {
    public:
        TortoiseDiffReporter() : FirstWorkingReporter(
                {new TortoiseTextDiffReporter(), new TortoiseImageDiffReporter()}) {
        }
    };

    class WinMergeReporter : public GenericDiffReporter {
    public:
        WinMergeReporter() : GenericDiffReporter(DiffPrograms::Windows::WIN_MERGE_REPORTER()) {}
    };

    class AraxisMergeReporter : public GenericDiffReporter {
    public:
        AraxisMergeReporter() : GenericDiffReporter(DiffPrograms::Windows::ARAXIS_MERGE()) {}
    };

    class CodeCompareReporter : public GenericDiffReporter {
    public:
        CodeCompareReporter() : GenericDiffReporter(DiffPrograms::Windows::CODE_COMPARE()) {}
    };

    class KDiff3Reporter : public GenericDiffReporter {
    public:
        KDiff3Reporter() : GenericDiffReporter(DiffPrograms::Windows::KDIFF3()) {}
    };

    class WindowsDiffReporter : public FirstWorkingReporter {
    public:
        WindowsDiffReporter() : FirstWorkingReporter(
                {
                        
                        new TortoiseDiffReporter(),
                        new BeyondCompareReporter(),
                        new WinMergeReporter(),
                        new AraxisMergeReporter(),
                        new CodeCompareReporter(),
                        new KDiff3Reporter(),
                        new VisualStudioCodeReporter(),
                        
                }
        ) {
        }
    };
}
}

#endif 

 // ******************** From: DiffReporter.h
#ifndef APPROVALTESTS_CPP_DIFFREPORTER_H
#define APPROVALTESTS_CPP_DIFFREPORTER_H


namespace ApprovalTests {
class DiffReporter : public FirstWorkingReporter
{
public:
    DiffReporter() : FirstWorkingReporter(
            {
                    new Mac::MacDiffReporter(),
                    new Linux::LinuxDiffReporter(),
                    new Windows::WindowsDiffReporter()
            }
    )
    {
    }
};
}

#endif 

 // ******************** From: DefaultReporterFactory.h
#ifndef APPROVALTESTS_CPP_DEFAULTREPORTERFACTORY_H
#define APPROVALTESTS_CPP_DEFAULTREPORTERFACTORY_H



namespace ApprovalTests {

class DefaultReporterFactory
{

private:
    static std::shared_ptr<Reporter>& defaultReporter()
    {
        static std::shared_ptr<Reporter> reporter = std::make_shared<DiffReporter>();
        return reporter;
    }

public:
    static std::shared_ptr<Reporter> getDefaultReporter()
    {
        return defaultReporter();
    }
    
    static void setDefaultReporter( const std::shared_ptr<Reporter>& reporter)
    {
        defaultReporter() = reporter;
    }


};
}

#endif 

 // ******************** From: DefaultReporterDisposer.h
#ifndef APPROVALTESTS_CPP_DEFAULTREPORTERDISPOSER_H
#define APPROVALTESTS_CPP_DEFAULTREPORTERDISPOSER_H


namespace ApprovalTests {

class APPROVAL_TESTS_NO_DISCARD DefaultReporterDisposer
{
private:
    std::shared_ptr<Reporter> previous_result;
public:
    explicit DefaultReporterDisposer(const std::shared_ptr<Reporter>& reporter)
    {
        previous_result = DefaultReporterFactory::getDefaultReporter();
        DefaultReporterFactory::setDefaultReporter(reporter);
    }

    ~DefaultReporterDisposer()
    {
        DefaultReporterFactory::setDefaultReporter(previous_result);
    }
};
}

#endif 

 // ******************** From: DefaultReporter.h
#ifndef APPROVALTESTS_CPP_DEFAULTREPORTER_H
#define APPROVALTESTS_CPP_DEFAULTREPORTER_H



namespace ApprovalTests {
class DefaultReporter : public Reporter
{
public:
    virtual bool report(std::string received, std::string approved) const override
    {
        return DefaultReporterFactory::getDefaultReporter()->report(received, approved);
    }
};
}

#endif 

 // ******************** From: SubdirectoryDisposer.h
#ifndef APPROVALTESTS_CPP_SUBDIRECTORYDISPOSER_H
#define APPROVALTESTS_CPP_SUBDIRECTORYDISPOSER_H



namespace ApprovalTests {

class APPROVAL_TESTS_NO_DISCARD SubdirectoryDisposer
{
private:
    std::string previous_result;
public:
    explicit SubdirectoryDisposer(std::string subdirectory)
    {
        previous_result = ApprovalTestNamer::testConfiguration().subdirectory;
        ApprovalTestNamer::testConfiguration().subdirectory = std::move(subdirectory);
    }

    ~SubdirectoryDisposer()
    {
        ApprovalTestNamer::testConfiguration().subdirectory = previous_result;
    }
};
}

#endif 

 // ******************** From: DefaultNamerFactory.h
#ifndef APPROVALTESTS_CPP_DEFAULTNAMERFACTORY_H
#define APPROVALTESTS_CPP_DEFAULTNAMERFACTORY_H



namespace ApprovalTests {

    using NamerCreator = std::function<std::shared_ptr<ApprovalNamer>()>;


class DefaultNamerFactory
{
private:
    static NamerCreator& defaultNamer()
    {
        static NamerCreator namer = [](){return std::make_shared<ApprovalTestNamer>();};
        return namer;
    }

public:
    static NamerCreator getDefaultNamer()
    {
        return defaultNamer();
    }
    
    static void setDefaultNamer( NamerCreator namer)
    {
        defaultNamer() = std::move(namer);
    }

};
}

#endif 

 // ******************** From: ExistingFileNamer.h
#ifndef APPROVALTESTS_CPP_EXISTINGFILENAMER_H
#define APPROVALTESTS_CPP_EXISTINGFILENAMER_H


namespace ApprovalTests {
class ExistingFileNamer: public ApprovalNamer{
    std::string filePath;
public:
    explicit ExistingFileNamer(std::string filePath): filePath(std::move(filePath)){

    }
    virtual std::string getApprovedFile(std::string extensionWithDot) const override {
        return DefaultNamerFactory::getDefaultNamer()()->getApprovedFile(extensionWithDot);
    }
    virtual std::string getReceivedFile(std::string ) const override {
        return filePath;
    }

};
}

#endif 

 // ******************** From: DefaultNamerDisposer.h
#ifndef APPROVALTESTS_CPP_DEFAULTNAMERDISPOSER_H
#define APPROVALTESTS_CPP_DEFAULTNAMERDISPOSER_H


namespace ApprovalTests {

class APPROVAL_TESTS_NO_DISCARD DefaultNamerDisposer
{
private:
    NamerCreator previous_result;
public:
    explicit DefaultNamerDisposer(NamerCreator namerCreator)
    {
        previous_result = DefaultNamerFactory::getDefaultNamer();
        DefaultNamerFactory::setDefaultNamer(std::move(namerCreator));
    }

    ~DefaultNamerDisposer()
    {
        DefaultNamerFactory::setDefaultNamer(previous_result);
    }
};
}

#endif 

 // ******************** From: QuietReporter.h
#ifndef APPROVALTESTS_CPP_QUIETREPORTER_H
#define APPROVALTESTS_CPP_QUIETREPORTER_H


namespace ApprovalTests {

class QuietReporter : public Reporter
{
public:
    bool report(std::string , std::string ) const override
    {
        return true;
    }
};
}

#endif 

 // ******************** From: CIBuildOnlyReporter.h
#ifndef APPROVALTESTS_CPP_CIBUILDONLYREPORTER_H
#define APPROVALTESTS_CPP_CIBUILDONLYREPORTER_H



namespace ApprovalTests
{
    
    class CIBuildOnlyReporter : public Reporter
    {
    private:
        std::shared_ptr<Reporter> m_reporter;

    public:
        explicit CIBuildOnlyReporter(std::shared_ptr<Reporter> reporter = std::make_shared<QuietReporter>())
            : m_reporter(reporter)
        {
        }

        bool report(std::string received, std::string approved) const override
        {
            if (!isRunningUnderCI())
            {
                return false;
            }
            m_reporter->report(received, approved);
            
            
            return true;
        }

        static bool isRunningUnderCI()
        {
            
            auto environmentVariablesForCI = {
                    
                    "CI",
                    "CONTINUOUS_INTEGRATION",
                    "GO_SERVER_URL",
                    "JENKINS_URL",
                    "TEAMCITY_VERSION"
                    
            };
            for (const auto& variable : environmentVariablesForCI)
            {
                if (!SystemUtils::safeGetEnv(variable).empty())
                {
                    return true;
                }
            }
            return false;
        }
    };
} 

#endif 

 // ******************** From: DefaultFrontLoadedReporter.h
#ifndef APPROVALTESTS_CPP_DEFAULTFRONTLOADEDREPORTER_H
#define APPROVALTESTS_CPP_DEFAULTFRONTLOADEDREPORTER_H


namespace ApprovalTests {
class DefaultFrontLoadedReporter : public FirstWorkingReporter
{
public:
    DefaultFrontLoadedReporter() : FirstWorkingReporter(
        {
            new CIBuildOnlyReporter()
        }
    )
    {
    }
};
}

#endif 

 // ******************** From: FrontLoadedReporterFactory.h
#ifndef APPROVALTESTS_CPP_FRONTLOADEDREPORTERFACTORY_H
#define APPROVALTESTS_CPP_FRONTLOADEDREPORTERFACTORY_H



namespace ApprovalTests {

class FrontLoadedReporterFactory
{
    static std::shared_ptr<Reporter>& frontLoadedReporter()
    {
        static std::shared_ptr<Reporter> reporter = std::make_shared<DefaultFrontLoadedReporter>();
        return reporter;
    }

public:
    static std::shared_ptr<Reporter> getFrontLoadedReporter()
    {
        return frontLoadedReporter();
    }

    static void setFrontLoadedReporter( const std::shared_ptr<Reporter>& reporter)
    {
        frontLoadedReporter() = reporter;
    }
};
}

#endif 

 // ******************** From: FrontLoadedReporterDisposer.h
#ifndef APPROVALTESTS_CPP_FRONTLOADEDREPORTERDISPOSER_H
#define APPROVALTESTS_CPP_FRONTLOADEDREPORTERDISPOSER_H


namespace ApprovalTests {

class APPROVAL_TESTS_NO_DISCARD FrontLoadedReporterDisposer
{
private:
    std::shared_ptr<Reporter> previous_result;
public:
    explicit FrontLoadedReporterDisposer(const std::shared_ptr<Reporter>& reporter)
    {
        previous_result = FrontLoadedReporterFactory::getFrontLoadedReporter();
        FrontLoadedReporterFactory::setFrontLoadedReporter(reporter);
    }

    ~FrontLoadedReporterDisposer()
    {
        FrontLoadedReporterFactory::setFrontLoadedReporter(previous_result);
    }

};
}

#endif 

 // ******************** From: ApprovalException.h
#ifndef APPROVALTESTS_CPP_APPROVALEXCEPTION_H
#define APPROVALTESTS_CPP_APPROVALEXCEPTION_H


namespace ApprovalTests {
class ApprovalException : public std::exception
{
private:
    std::string message;
public:
    explicit ApprovalException( const std::string& msg ) : message( msg ) {}

    virtual const char *what() const noexcept override
    {
        return message.c_str();
    }
};

class ApprovalMismatchException : public ApprovalException
{
private:
    std::string format( const std::string &received, const std::string &approved )
    {
        std::stringstream s;
        s << "Failed Approval: \n"
          << "Received does not match approved \n"
          << "Received : \"" << received << "\" \n"
          << "Approved : \"" << approved << "\"";
        return s.str();
    }
public:
    ApprovalMismatchException(const std::string& received, const std::string& approved )
        : ApprovalException( format( received, approved ) )
    {
    }
};

class ApprovalMissingException : public ApprovalException
{
private:
    std::string format( const std::string &file )
    {
        std::stringstream s;
        s << "Failed Approval: \n"
          << "Approval File Not Found \n"
          << "File: \"" << file << '"';
        return s.str();
    }
public:
    ApprovalMissingException(const std::string& , const std::string& approved )
        : ApprovalException( format( approved ) )
    {
    }
};
}

#endif

 // ******************** From: ApprovalComparator.h
#ifndef APPROVALTESTS_CPP_APPROVALCOMPARATOR_H
#define APPROVALTESTS_CPP_APPROVALCOMPARATOR_H


namespace ApprovalTests {
class ApprovalComparator
{
public:
    virtual ~ApprovalComparator() = default;

    virtual bool contentsAreEquivalent(std::string receivedPath,
                                       std::string approvedPath) const = 0;
};
}

#endif 

 // ******************** From: TextFileComparator.h
#ifndef APPROVALTESTS_CPP_TEXTFILECOMPARATOR_H
#define APPROVALTESTS_CPP_TEXTFILECOMPARATOR_H


namespace ApprovalTests {
class TextFileComparator : public ApprovalComparator
{
public:
    static std::ifstream::int_type getNextRelevantCharacter(std::ifstream& astream)
    {
        auto ch = astream.get();
        if (ch == '\r')
        {
            return astream.get();
        }
        else
        {
            return ch;
        }
    }

    virtual bool contentsAreEquivalent(std::string receivedPath,
                                       std::string approvedPath) const override
    {
        std::ifstream astream(approvedPath.c_str(),
                              std::ios::binary | std::ifstream::in);
        std::ifstream rstream(receivedPath.c_str(),
                              std::ios::binary | std::ifstream::in);

        while (astream.good() && rstream.good()) {
            int a = getNextRelevantCharacter(astream);
            int r = getNextRelevantCharacter(rstream);

            if (a != r) {
                return false;
            }
        }
        return true;
    }
};
}
#endif 

 // ******************** From: ComparatorDisposer.h
#ifndef APPROVALTESTS_CPP_COMPARATORDISPOSER_H
#define APPROVALTESTS_CPP_COMPARATORDISPOSER_H


namespace ApprovalTests
{

using ComparatorContainer = std::map<std::string, std::shared_ptr<ApprovalComparator> >;

class APPROVAL_TESTS_NO_DISCARD ComparatorDisposer
{
public:
    ComparatorDisposer(
            ComparatorContainer &comparators,
            std::string extensionWithDot,
            std::shared_ptr<ApprovalTests::ApprovalComparator> previousComparator,
            std::shared_ptr<ApprovalTests::ApprovalComparator> newComparator)
            :
            comparators(comparators),
            ext_(extensionWithDot),
            previousComparator(previousComparator)
    {
        comparators[extensionWithDot] = newComparator;
    }

    ~ComparatorDisposer()
    {
        comparators[ext_] = previousComparator;
    }

private:
    ComparatorContainer &comparators;
    std::string ext_;
    std::shared_ptr<ApprovalTests::ApprovalComparator> previousComparator;
};

}

#endif 

 // ******************** From: ComparatorFactory.h
#ifndef APPROVALTESTS_CPP_COMPARATORFACTORY_H
#define APPROVALTESTS_CPP_COMPARATORFACTORY_H


namespace ApprovalTests {

class ComparatorFactory {
private:
    static ComparatorContainer &comparators() {
        static ComparatorContainer allComparators;
        return allComparators;
    }

public:
    static ComparatorDisposer
    registerComparator(const std::string &extensionWithDot, std::shared_ptr<ApprovalComparator> comparator) {
        return ComparatorDisposer(comparators(), extensionWithDot,
                                  getComparatorForFileExtensionWithDot(extensionWithDot),
                                  comparator);
    }

    static std::shared_ptr<ApprovalComparator> getComparatorForFile(const std::string &receivedPath) {
        const std::string fileExtension = FileUtils::getExtensionWithDot(receivedPath);
        return getComparatorForFileExtensionWithDot(fileExtension);
    }

    static std::shared_ptr<ApprovalComparator>
    getComparatorForFileExtensionWithDot(const std::string &fileExtensionWithDot) {
        auto iterator = comparators().find(fileExtensionWithDot);
        if (iterator != comparators().end()) {
            return iterator->second;
        }
        return std::make_shared<TextFileComparator>();
    }
};

}

#endif 

 // ******************** From: FileApprover.h
#ifndef APPROVALTESTS_CPP_FILEAPPROVER_H
#define APPROVALTESTS_CPP_FILEAPPROVER_H


namespace ApprovalTests {

class FileApprover {

public:
    FileApprover() = default;

    ~FileApprover() = default;

    static ComparatorDisposer registerComparatorForExtension(const std::string& extensionWithDot, std::shared_ptr<ApprovalComparator> comparator)
    {
        return ComparatorFactory::registerComparator(extensionWithDot, comparator);
    }

    
    static void verify(const std::string& receivedPath,
                       const std::string& approvedPath,
                       const ApprovalComparator& comparator) {
        if (!FileUtils::fileExists(approvedPath)) {
            throw ApprovalMissingException(receivedPath, approvedPath);
        }

        if (!FileUtils::fileExists(receivedPath)) {
            throw ApprovalMissingException(approvedPath, receivedPath);
        }

        if (!comparator.contentsAreEquivalent(receivedPath, approvedPath)) {
            throw ApprovalMismatchException(receivedPath, approvedPath);
        }
    }

    static void verify(const std::string& receivedPath,
                       const std::string& approvedPath) {
        verify(receivedPath, approvedPath, *ComparatorFactory::getComparatorForFile(receivedPath));
    }

    static void verify(const ApprovalNamer& n, const ApprovalWriter& s, const Reporter& r) {
        std::string approvedPath = n.getApprovedFile(s.getFileExtensionWithDot());
        std::string receivedPath = n.getReceivedFile(s.getFileExtensionWithDot());
        s.write(receivedPath);
        try
        {
            verify(receivedPath, approvedPath);
            s.cleanUpReceived(receivedPath);
        }
        catch (const ApprovalException&) {
            reportAfterTryingFrontLoadedReporter(receivedPath, approvedPath, r);
            throw;
        }
    }

    static void
    reportAfterTryingFrontLoadedReporter(const std::string &receivedPath, const std::string &approvedPath, const Reporter &r)
    {
        auto tryFirst = FrontLoadedReporterFactory::getFrontLoadedReporter();
        if (!tryFirst->report(receivedPath, approvedPath))
        {
            r.report(receivedPath, approvedPath);
        }
    }


};
}

#endif

 // ******************** From: Approvals.h
#ifndef APPROVALTESTS_CPP_APPROVALS_H
#define APPROVALTESTS_CPP_APPROVALS_H


namespace ApprovalTests {
class Approvals {
private:
    Approvals() = default;

    ~Approvals() = default;

public:
    static std::shared_ptr<ApprovalNamer> getDefaultNamer()
    {
        return DefaultNamerFactory::getDefaultNamer()();
    }

    static void verify(std::string contents, const Reporter &reporter = DefaultReporter()) {
        verifyWithExtension(contents, ".txt", reporter);
    }

    static void verifyWithExtension(std::string contents, const std::string& fileExtensionWithDot, const Reporter &reporter = DefaultReporter()) {
        StringWriter writer(contents, fileExtensionWithDot);
        FileApprover::verify(*getDefaultNamer(), writer, reporter);
    }

    static void verify(const ApprovalWriter& writer, const Reporter &reporter = DefaultReporter())
    {
        FileApprover::verify(*getDefaultNamer(), writer, reporter);
    }

    template<typename T>
    using IsNotDerivedFromWriter = typename std::enable_if<!std::is_base_of<ApprovalWriter, T>::value, int>::type;

    template<
            typename T,
            typename = IsNotDerivedFromWriter<T>>
    static void verify(const T& contents, const Reporter &reporter = DefaultReporter()) {
        verify(StringUtils::toString(contents), reporter);
    }

    template<
            typename T,
            typename = IsNotDerivedFromWriter<T>>
    static void verifyWithExtension(const T& contents, const std::string& fileExtensionWithDot, const Reporter &reporter = DefaultReporter()) {
        verifyWithExtension(StringUtils::toString(contents), fileExtensionWithDot, reporter);
    }

    template<
        typename T,
        typename Function,
        typename = IsNotDerivedFromReporter<Function>>
    static void verify(const T& contents,
                       Function converter,
                       const Reporter &reporter = DefaultReporter())
    {
        std::stringstream s;
        converter(contents, s);
        verify(s.str(), reporter);
    }

    template<
        typename T,
        typename Function,
        typename = IsNotDerivedFromReporter<Function>>
    static void verifyWithExtension(const T& contents,
                       Function converter,
                       const std::string& fileExtensionWithDot,
                       const Reporter &reporter = DefaultReporter())
    {
        std::stringstream s;
        converter(contents, s);
        verifyWithExtension(s.str(), fileExtensionWithDot, reporter);
    }

    static void verifyExceptionMessage(
        std::function<void(void)> functionThatThrows,
        const Reporter &reporter = DefaultReporter())
    {
        std::string message = "*** no exception thrown ***";
        try
        {
            functionThatThrows();
        }
        catch(const std::exception& e)
        {
            message = e.what();
        }
        verify(message, reporter);
    }

    template<typename Iterator>
    static void verifyAll(std::string header,
                          const Iterator &start, const Iterator &finish,
                          std::function<void(typename Iterator::value_type, std::ostream &)> converter,
                          const Reporter &reporter = DefaultReporter()) {
        std::stringstream s;
        if (!header.empty()) {
            s << header << "\n\n\n";
        }
        for (auto it = start; it != finish; ++it) {
            converter(*it, s);
            s << '\n';
        }
        verify(s.str(), reporter);
    }

    template<typename Container>
    static void verifyAll(std::string header,
                          const Container &list,
                          std::function<void(typename Container::value_type, std::ostream &)> converter,
                          const Reporter &reporter = DefaultReporter()) {
        verifyAll<typename Container::const_iterator>(header, list.begin(), list.end(), converter, reporter);
    }

    template<typename T>
    static void verifyAll(std::string header,
                          const std::vector<T> &list,
                          const Reporter &reporter = DefaultReporter()) {
        int i = 0;
        verifyAll<std::vector<T>>(header, list, [&](T e, std::ostream &s) { s << "[" << i++ << "] = " << e; },
                                  reporter);
    }

    template<typename T>
    static void verifyAll(const std::vector<T> &list,
                          const Reporter &reporter = DefaultReporter()) {
        verifyAll<T>("", list, reporter);
    }

    static void verifyExistingFile(const std::string filePath, const Reporter &reporter = DefaultReporter()) {
        ExistingFile writer(filePath);
        ExistingFileNamer namer(filePath);
        FileApprover::verify(namer, writer, reporter);
    }

    static SubdirectoryDisposer useApprovalsSubdirectory(std::string subdirectory = "approval_tests")
    {
        return SubdirectoryDisposer(subdirectory);
    }

    static DefaultReporterDisposer useAsDefaultReporter(const std::shared_ptr<Reporter>& reporter)
    {
        return DefaultReporterDisposer(reporter);
    }

    static FrontLoadedReporterDisposer useAsFrontLoadedReporter(const std::shared_ptr<Reporter>& reporter)
    {
        return FrontLoadedReporterDisposer(reporter);
    }

    static DefaultNamerDisposer useAsDefaultNamer(NamerCreator namerCreator)
    {
        return DefaultNamerDisposer(namerCreator);
    }

};
}

#endif

 // ******************** From: CombinationApprovals.h
#ifndef APPROVALTESTS_CPP_COMBINATIONAPPROVALS_H
#define APPROVALTESTS_CPP_COMBINATIONAPPROVALS_H


namespace ApprovalTests {
namespace CombinationApprovals {
namespace Detail {




template<class...> struct disjunction : std::false_type {};
template<class B1> struct disjunction<B1> : B1 {};
template<class B1, class... Bn>
struct disjunction<B1, Bn...> : std::conditional<bool(B1::value), B1, disjunction<Bn...>>::type  {};



struct print_input {
    std::ostream& out;
    template<class T>
    void operator()(const T& input) {
        out << ", " << input;
    }
};


template<class Converter>
struct serialize {
    std::ostream& out;
    Converter converter;
    template<class T, class... Ts>
    void operator()(T&& input1, Ts&&... inputs) {
        
        out << "(" << input1;
        
        CartesianProduct::Detail::for_each(std::forward_as_tuple(inputs...), print_input{out});
        out << ") => " << converter(input1, inputs...) << '\n';
    }
};
} 

template<class Converter, class Container, class... Containers>
void verifyAllCombinations(Converter&& converter, const Reporter& reporter, const Container& input0, const Containers&... inputs)
{
    std::stringstream s;
    CartesianProduct::cartesian_product(Detail::serialize<Converter>{s, std::forward<Converter>(converter)}, input0, inputs...);
    Approvals::verify(s.str(), reporter);
}

template<class Converter, class... Containers>
CartesianProduct::Detail::enable_if_t<!Detail::disjunction<std::is_base_of<Reporter, Containers>...>::value>
verifyAllCombinations(Converter&& converter, const Containers&... inputs)
{
    verifyAllCombinations(std::forward<Converter>(converter), DefaultReporter(), inputs...);
}

} 
} 

#endif

 // ******************** From: Catch2Approvals.h

#ifndef APPROVALTESTS_CPP_CATCH2APPROVALS_H
#define APPROVALTESTS_CPP_CATCH2APPROVALS_H


// <SingleHpp unalterable>
#if defined(APPROVALS_CATCH_EXISTING_MAIN)
    #define APPROVALS_CATCH
    #define CATCH_CONFIG_RUNNER
#elif defined(APPROVALS_CATCH)
    #define CATCH_CONFIG_MAIN
#endif

#ifdef APPROVALS_CATCH

#include <Catch.hpp>

//namespace ApprovalTests {
struct Catch2ApprovalListener : Catch::TestEventListenerBase {
    ApprovalTests::TestName currentTest;
    using TestEventListenerBase::TestEventListenerBase; // This using allows us to use all base-class constructors
    virtual void testCaseStarting(Catch::TestCaseInfo const &testInfo) override {

        currentTest.setFileName(testInfo.lineInfo.file);
        ApprovalTests::ApprovalTestNamer::currentTest(&currentTest);
    }

    virtual void testCaseEnded(Catch::TestCaseStats const &/*testCaseStats*/) override {
        while (!currentTest.sections.empty()) {
            currentTest.sections.pop_back();
        }
    }

    virtual void sectionStarting(Catch::SectionInfo const &sectionInfo) override {
        currentTest.sections.push_back(sectionInfo.name);
    }

    virtual void sectionEnded(Catch::SectionStats const &/*sectionStats*/) override {
        currentTest.sections.pop_back();
    }
};
//}
CATCH_REGISTER_LISTENER(Catch2ApprovalListener)

#endif
#ifdef TEST_COMMIT_REVERT_CATCH

//namespace ApprovalTests {
struct Catch2TestCommitRevert : Catch::TestEventListenerBase {
    using TestEventListenerBase::TestEventListenerBase; // This using allows us to use all base-class constructors
    virtual void  testRunEnded( Catch::TestRunStats const& testRunStats )override{
        bool commit = testRunStats.totals.testCases.allOk();
        std::string message = "r ";
        if (commit) {
            std::cout << "git add -A n";
            std::cout << "git commit -m " << message;
        } else
        {
            std::cout << "git clean -fd n";
            std::cout << "git reset --hard HEAD n";
        }
    }
};
//}
CATCH_REGISTER_LISTENER(Catch2TestCommitRevert)
#endif
// </SingleHpp>
#endif 

 // ******************** From: DocTestApprovals.h
#ifndef APPROVALTESTS_CPP_DOCTESTAPPROVALS_H
#define APPROVALTESTS_CPP_DOCTESTAPPROVALS_H


// <SingleHpp unalterable>
#ifdef APPROVALS_DOCTEST

#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN

#include <doctest.h>

namespace ApprovalTests {
// anonymous namespace to prevent compiler -Wsubobject-linkage warnings
// This is OK as this code is only compiled on main()
namespace {
    struct AbstractReporter : doctest::IReporter {
        virtual void report_query(const doctest::QueryData&) override {}
        // called when the whole test run starts
        virtual void test_run_start() override {}

        // called when the whole test run ends (caching a pointer to the input doesn't make sense here)
        virtual void test_run_end(const doctest::TestRunStats &) override {}

        // called when a test case is started (safe to cache a pointer to the input)
        virtual void test_case_start(const doctest::TestCaseData &) override {}

#if 20305 <= DOCTEST_VERSION
        // called when a test case is reentered because of unfinished subcases (safe to cache a pointer to the input)
        virtual void test_case_reenter(const doctest::TestCaseData&) override {}
#endif

        // called when a test case has ended
        virtual void test_case_end(const doctest::CurrentTestCaseStats &) override {}

        // called when an exception is thrown from the test case (or it crashes)
        virtual void test_case_exception(const doctest::TestCaseException &) override {}

        // called whenever a subcase is entered (don't cache pointers to the input)
        virtual void subcase_start(const doctest::SubcaseSignature &) override {}

        // called whenever a subcase is exited (don't cache pointers to the input)
        virtual void subcase_end() override {}

        // called for each assert (don't cache pointers to the input)
        virtual void log_assert(const doctest::AssertData &) override {}

        // called for each message (don't cache pointers to the input)
        virtual void log_message(const doctest::MessageData &) override {}

        // called when a test case is skipped either because it doesn't pass the filters, has a skip decorator
        // or isn't in the execution range (between first and last) (safe to cache a pointer to the input)
        virtual void test_case_skipped(const doctest::TestCaseData &) override {}


    };

    struct DocTestApprovalListener : AbstractReporter {
        TestName currentTest;

        // constructor has to accept the ContextOptions by ref as a single argument
        explicit DocTestApprovalListener(const doctest::ContextOptions & /*in*/) {
        }

        void test_case_start(const doctest::TestCaseData &testInfo) override {

            currentTest.sections.emplace_back(testInfo.m_name);
            currentTest.setFileName(testInfo.m_file);
            ApprovalTestNamer::currentTest(&currentTest);
        }

        void test_case_end(const doctest::CurrentTestCaseStats & /*in*/) override {

            while (!currentTest.sections.empty()) {
                currentTest.sections.pop_back();
            }
        }

        void subcase_start(const doctest::SubcaseSignature &signature) override {

            currentTest.sections.emplace_back(signature.m_name);
        }

        void subcase_end() override {

            currentTest.sections.pop_back();
        }
    };
}
}

REGISTER_LISTENER("approvals", 0, ApprovalTests::DocTestApprovalListener);


#endif // APPROVALS_DOCTEST
// </SingleHpp>
#endif 

 // ******************** From: GoogleConfiguration.h
#ifndef APPROVALTESTS_CPP_GOOGLECONFIGURATION_H
#define APPROVALTESTS_CPP_GOOGLECONFIGURATION_H


namespace ApprovalTests {
class GoogleConfiguration
{
public:
    
    APPROVAL_TESTS_NO_DISCARD static bool addTestCaseNameRedundancyCheck(GoogleCustomizationsFactory::Comparator comparator)
    {
        return GoogleCustomizationsFactory::addTestCaseNameRedundancyCheck(comparator);
    }

    
    APPROVAL_TESTS_NO_DISCARD static bool addIgnorableTestCaseNameSuffix(std::string suffix)
    {
        return addTestCaseNameRedundancyCheck( createIgnorableTestCaseNameSuffixCheck(suffix) );
    }

    static GoogleCustomizationsFactory::Comparator createIgnorableTestCaseNameSuffixCheck( const std::string& suffix )
    {
        return [suffix](std::string testFileNameWithExtension, std::string testCaseName)
        {
            if (testCaseName.length() <= suffix.length() || !StringUtils::endsWith(testCaseName, suffix))
            {
                return false;
            }

            auto withoutSuffix = testCaseName.substr(0, testCaseName.length() - suffix.length());
            auto withFileExtension = withoutSuffix + ".";
            return StringUtils::contains(testFileNameWithExtension, withFileExtension);
        };
    }
};
}

#endif 

 // ******************** From: GoogleTestApprovals.h
#ifndef APPROVALTESTS_CPP_GOOGLTESTAPPPROVALS_H
#define APPROVALTESTS_CPP_GOOGLTESTAPPPROVALS_H


#ifdef APPROVALS_GOOGLETEST_EXISTING_MAIN
#define APPROVALS_GOOGLETEST
#endif

#ifdef APPROVALS_GOOGLETEST

// <SingleHpp unalterable>
#include "gtest/gtest.h"

namespace ApprovalTests {
class GoogleTestListener : public testing::EmptyTestEventListener
{
    TestName currentTest;
public:
    bool isDuplicate(std::string testFileNameWithExtension, std::string testCaseName)
    {
        for( auto check : GoogleCustomizationsFactory::getEquivalencyChecks())
        {
            if (check(testFileNameWithExtension, testCaseName))
            {
                return true;
            }
        }
        return false;
    }

    virtual void OnTestStart(const testing::TestInfo& testInfo) override
    {
        currentTest.setFileName(testInfo.file());
        currentTest.sections = {};
        if (! isDuplicate(currentTest.getFileName(), testInfo.test_case_name()))
        {
            currentTest.sections.emplace_back(testInfo.test_case_name());
        }
        if (! std::string(testInfo.name()).empty())
        {
            currentTest.sections.emplace_back(testInfo.name());
        }
        
        ApprovalTestNamer::currentTest(&currentTest);
    }
};

inline void initializeApprovalTestsForGoogleTests() {
    auto& listeners = testing::UnitTest::GetInstance()->listeners();
    listeners.Append(new GoogleTestListener);
}
}

#ifndef APPROVALS_GOOGLETEST_EXISTING_MAIN
int main(int argc, char** argv)
{
    ::testing::InitGoogleTest(&argc, argv);
    ApprovalTests::initializeApprovalTestsForGoogleTests();
    return RUN_ALL_TESTS();
}
#endif //APPROVALS_GOOGLETEST_EXISTING_MAIN

// </SingleHpp>
#endif
#endif 

 // ******************** From: NamerFactory.h
#ifndef APPROVALTESTS_CPP_NAMERFACTORY_H
#define APPROVALTESTS_CPP_NAMERFACTORY_H



namespace ApprovalTests {
struct NamerFactory
{
    static SectionNameDisposer appendToOutputFilename(const std::string& sectionName)
    {
        return SectionNameDisposer(ApprovalTestNamer::currentTest(), sectionName);
    }
};
}

#endif 

 // ******************** From: SeparateApprovedAndReceivedDirectoriesNamer.h
#ifndef APPROVALTESTS_CPP_SEPARATEAPPROVEDANDRECEIVEDDIRECTORIESNAMER_H
#define APPROVALTESTS_CPP_SEPARATEAPPROVEDANDRECEIVEDDIRECTORIESNAMER_H


namespace ApprovalTests {
class SeparateApprovedAndReceivedDirectoriesNamer : public ApprovalTestNamer
{
public:
    virtual ~SeparateApprovedAndReceivedDirectoriesNamer() = default;

    std::string getFullFileNameWithExtraDirectory(const std::string& approved, const std::string& extensionWithDot) const
    {
        std::string outputDirectory = getDirectory() +  approved;
        SystemUtils::ensureDirectoryExists(outputDirectory);
    
        std::string outputFile = getFileName() + "." + getTestName() + extensionWithDot;
    
        return outputDirectory + SystemUtils::getDirectorySeparator() + outputFile;
    }
    
    virtual std::string getApprovedFile(std::string extensionWithDot) const override
    {
        return getFullFileNameWithExtraDirectory("approved", extensionWithDot);
    }
    
    virtual std::string getReceivedFile(std::string extensionWithDot) const override
    {
        return getFullFileNameWithExtraDirectory("received", extensionWithDot);
    }
    
    static DefaultNamerDisposer useAsDefaultNamer()
    {
        return Approvals::useAsDefaultNamer([](){return std::make_shared<SeparateApprovedAndReceivedDirectoriesNamer>();});
    }

};
}

#endif 

 // ******************** From: AutoApproveIfMissingReporter.h
#ifndef APPROVALTESTS_CPP_AUTOAPPROVEIFMISSINGREPORTER_H
#define APPROVALTESTS_CPP_AUTOAPPROVEIFMISSINGREPORTER_H


namespace ApprovalTests {
class AutoApproveIfMissingReporter : public Reporter
{
public:
    bool report(std::string received, std::string approved) const override
    {
        if (FileUtils::fileExists(approved))
        {
            return false;
        }

        return AutoApproveReporter().report(received, approved);
    }
};
}

#endif 

 // ******************** From: BlockingReporter.h
#ifndef APPROVALTESTS_CPP_BLOCKINGREPORTER_H
#define APPROVALTESTS_CPP_BLOCKINGREPORTER_H



namespace ApprovalTests {
class BlockingReporter : public Reporter
{
private:
    std::shared_ptr<Blocker> blocker;

    BlockingReporter() = delete;

public:
    explicit BlockingReporter( std::shared_ptr<Blocker> blocker ) : blocker(std::move(blocker))
    {
    }

    static std::shared_ptr<BlockingReporter> onMachineNamed( const std::string& machineName )
    {
        auto machineBlocker = std::make_shared<MachineBlocker>( MachineBlocker::onMachineNamed(machineName) );
        return std::make_shared<BlockingReporter>(machineBlocker);
    }

    static std::shared_ptr<BlockingReporter> onMachinesNotNamed( const std::string& machineName )
    {
        auto machineBlocker = std::make_shared<MachineBlocker>( MachineBlocker::onMachinesNotNamed(machineName) );
        return std::make_shared<BlockingReporter>(machineBlocker);
    }

    virtual bool report(std::string , std::string ) const override
    {
        return blocker->isBlockingOnThisMachine();
    }
};
}

#endif 

 // ******************** From: CIBuildOnlyReporterUtils.h
#ifndef APPROVALTESTS_CPP_CIBUILDONLYREPORTERUTILS_H
#define APPROVALTESTS_CPP_CIBUILDONLYREPORTERUTILS_H


namespace ApprovalTests
{
    namespace CIBuildOnlyReporterUtils
    {
        inline FrontLoadedReporterDisposer useAsFrontLoadedReporter(const std::shared_ptr<Reporter>& reporter)
        {
            return Approvals::useAsFrontLoadedReporter(
                    std::make_shared<ApprovalTests::CIBuildOnlyReporter>( reporter ));
        }
    }
} 

#endif 

 // ******************** From: ClipboardReporter.h
#ifndef APPROVALTESTS_CPP_COMMANDLINEREPORTER_H
#define APPROVALTESTS_CPP_COMMANDLINEREPORTER_H




namespace ApprovalTests {
class ClipboardReporter : public Reporter {
public:
    static std::string getCommandLineFor(const std::string& received, const std::string& approved, bool isWindows)
    {
        if (isWindows) {
            return std::string("move /Y ") + "\"" + received + "\" \"" + approved + "\"";
        } else {
            return std::string("mv ") + "\"" + received + "\" \"" + approved + "\"";
        }
    }

    virtual bool report(std::string received, std::string approved) const override
    {
        copyToClipboard(getCommandLineFor(received, approved, SystemUtils::isWindowsOs()));
        return true;
    }

    static void copyToClipboard(const std::string& newClipboard) {
        

        const std::string clipboardCommand = SystemUtils::isWindowsOs() ? "clip" : "pbclip";
        auto cmd = std::string("echo ") + newClipboard + " | " + clipboardCommand;
        system(cmd.c_str());
    }
};
}

#endif 

 // ******************** From: CombinationReporter.h
#ifndef APPROVALTESTS_CPP_COMBINATIONREPORTER_H
#define APPROVALTESTS_CPP_COMBINATIONREPORTER_H


namespace ApprovalTests {
class CombinationReporter : public Reporter
{
private:
    std::vector< std::unique_ptr<Reporter> > reporters;
public:
    
    explicit CombinationReporter(const std::vector<Reporter*>& theReporters)
    {
        for(auto r : theReporters)
        {
            reporters.push_back(std::unique_ptr<Reporter>(r));
        }
    }

    bool report(std::string received, std::string approved) const override
    {
        bool result = false;
        for(auto& r : reporters)
        {
            result |= r->report(received, approved);
        }
        return result;
    }
};
}

#endif 

 // ******************** From: ExceptionCollector.h
#ifndef APPROVALTESTS_CPP_EXCEPTIONCOLLECTOR_H
#define APPROVALTESTS_CPP_EXCEPTIONCOLLECTOR_H


namespace ApprovalTests {
class ExceptionCollector
{
    std::vector<std::string> exceptionMessages;

public:
    void gather(std::function<void(void)> functionThatThrows)
    {
        try
        {
            functionThatThrows();
        }
        catch(const std::exception& e)
        {
            exceptionMessages.emplace_back(e.what());
        }
    }
    ~ExceptionCollector()
    {
        if ( ! exceptionMessages.empty())
        {
            exceptionMessages.emplace_back("ERROR: Calling code forgot to call exceptionCollector.release()");
        }
        release();
    }

    void release()
    {
        if (! exceptionMessages.empty())
        {
            std::stringstream s;
            s << exceptionMessages.size() << " exceptions were thrown:\n\n";
            int count = 1;
            for( const auto& error : exceptionMessages)
            {
                s << count++ << ") " << error << '\n';
            }
            exceptionMessages.clear();
            throw std::runtime_error(s.str());
        }
    }
};
}

#endif 

