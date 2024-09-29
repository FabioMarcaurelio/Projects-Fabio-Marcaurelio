#include <iostream>

void printMenu() //return void
{
    
    std::cout << "1: print help" <<std::endl;

    std::cout << "2: print exchange stats" <<std::endl;

    std::cout << "3: make an offer" <<std::endl;

    std::cout << "4: make a bid" <<std::endl;

    std::cout << "5: print a wallet" <<std::endl;

    std::cout << "6: continue" <<std::endl;

    std::cout << "===============" << std::endl;
        
}

void invalidinf()
{
    std::cout << "Invalid choice. Choose 1-6" << std::endl;
    std::cout << "and follow the on screen instructions." << std::endl;
}


void printHelp()
{
    std::cout <<"help - your aim is to make money, make bid and offers" << std::endl;

}

void printMarketstats()
{
    std::cout << "market looks good" << std::endl;
}

void enterOffer()
{
    std::cout <<"make an offer - enter the ammount" << std::endl;

}

void enterBid()
{
    std::cout <<"make a bid - enter the ammount" << std::endl;

}

void printWallet()
{
    std::cout <<"your wallet is empty" << std::endl;
}

void  nextframework()
{
    std::cout <<"going to next time frame" << std::endl;
}

void invalidsup()
{
    std::cout << "Invalid choice. Choose 1-6" << std::endl;
}



int getUserOption()
{
    int userOption;

    std::cout << "Type in 1-6" << std::endl;
    std::cin >> userOption;
    std::cout << "You chose " << userOption << std::endl;
    return userOption;
}

void processUserOption(int userOption)
{
    if (userOption < 1)
    {
        invalidinf();
    }
    if (userOption == 1) 
    {
        printHelp();
    }
    if (userOption == 2) 
    {
        printMarketstats();
    }
    if (userOption == 3) 
    {
        enterOffer();
    }
    if (userOption == 4) 
    {
        enterBid();
    }
    if (userOption == 5) 
    {
        printWallet();
    }
    if (userOption == 6) 
    {
        nextframework();
    }
    if (userOption > 6) 
    {
        invalidsup();
    }

}


int main()
{
    while (true)
    {
        printMenu();
        int userOption = getUserOption();
        processUserOption(userOption);
    }
    return 0;
}
