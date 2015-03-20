LoadPackages = function()
  ################################################################################
# This function loads all the packages required for supervised learning Workflow
################################################################################
{
  if(!require(hash))
  {
    install.packages(hash);
    library(hash);
  }
  
  if(!require(caret))
  {
    install.packages(caret);
    library(caret);
  }
  
  if(!require(ada))
  {
    install.packages(ada);
    library(ada);
  }
  
  if(!require(caTools))
  {
    install.packages(caTools);
    library(caTools);
  }
  
  if(!require(gbm))
  {
    install.packages(gbm);
    library(gbm);
  }
}