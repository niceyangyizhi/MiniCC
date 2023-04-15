#include <memory>
#include <vector>
#include <iostream>

class Object
{
public:
    virtual ~Object() {}
};

class Base1 : public Object
{
public:
    virtual void foo() { std::cout << "Base1::foo()\n"; }
};

class Base2 : public Object
{
public:
    virtual void bar() { std::cout << "Base2::bar()\n"; }
};

class Derived : public Base1, public Base2
{
public:
    virtual void baz() { std::cout << "Derived::baz()\n"; }
};

int main()
{
    std::unique_ptr<Derived> derivedPtr = std::make_unique<Derived>();

    // 将 Derived 指针转换为 Base1 指针
    std::unique_ptr<Base1> base1Ptr = std::move(derivedPtr);
    base1Ptr->foo(); // 调用 Base1::foo()

    derivedPtr = std::make_unique<Derived>();
    // 将 Derived 指针转换为 Base2 指针
    std::unique_ptr<Base2> base2Ptr = std::move(derivedPtr);
    base2Ptr->bar(); // 调用 Base2::bar()

    std::vector<std::unique_ptr<Base2>> base1Vec;
    derivedPtr = std::make_unique<Derived>();

    return 0;
}