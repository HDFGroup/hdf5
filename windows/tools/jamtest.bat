
@REM Should copy all testing files to this directory
@REM This batch file only tests the validation of h5jam and h5unjam tools on windows,
@REM It will not check the correctedness of jammed/unjammed user-block.
cd h5jam%2\%1 
echo off
copy /Y ..\..\testfiles\tall.h5 tall.h5 >temp.txt
copy /Y ..\..\testfiles\twithub.h5 twithub.h5 >temp.txt
copy /Y ..\..\testfiles\twithub513.h5 twithub513.h5 >temp.txt
copy /Y ..\..\testfiles\u10.txt u10.txt >temp.txt
copy /Y ..\..\testfiles\u511.txt u511.txt >temp.txt
copy /Y ..\..\testfiles\u512.txt u512.txt >temp.txt
copy /Y ..\..\testfiles\u513.txt u513.txt >temp.txt

h5jam -u u10.txt -i tall.h5 -o ta2.h5             
echo h5jam -u u10.txt -i tall.h5 -o ta2.h5                                    PASSED             
h5jam -u u511.txt -i tall.h5 -o ta3.h5     
echo h5jam -u u511.txt -i tall.h5 -o ta3.h5                                   PASSED                    
h5jam -u u512.txt -i tall.h5 -o ta4.h5  
echo h5jam -u u512.txt -i tall.h5 -o ta4.h5                                   PASSED                       
h5jam -u u513.txt -i tall.h5 -o ta5.h5    
echo h5jam -u u513.txt -i tall.h5 -o ta5.h5                                   PASSED    
copy /Y tall.h5 ta.h5 >temp.txt             
h5jam -u u10.txt -i ta.h5  
echo h5jam -u u10.txt -i ta.h5                                                PASSED
copy /Y tall.h5 ta.h5 >temp.txt                                      
h5jam -u u511.txt -i ta.h5                                                    
echo h5jam -u u511.txt -i ta.h5	                                         PASSED
copy /Y tall.h5 ta.h5 >temp.txt                                      
h5jam -u u512.txt -i ta.h5                                                    
echo h5jam -u u512.txt -i ta.h5                                               PASSED
copy /Y tall.h5 ta.h5 >temp.txt                                     
h5jam -u u513.txt -i ta.h5                                                    
echo h5jam -u u513.txt -i ta.h5                                               PASSED                                   
h5jam -u u10.txt -i twithub.h5 -o tax2.h5   	                              
echo h5jam -u u10.txt -i twithub.h5 -o tax2.h5                                PASSED                  
h5jam -u u511.txt -i twithub.h5 -o tax3.h5                                    
echo h5jam -u u511.txt -i twithub.h5 -o tax3.h5                               PASSED            
h5jam -u u512.txt -i twithub.h5 -o tax4.h5                                    
echo h5jam -u u512.txt -i twithub.h5 -o tax4.h5                               PASSED              
h5jam -u u513.txt -i twithub.h5 -o tax5.h5                                    
echo h5jam -u u513.txt -i twithub.h5 -o tax5.h5                               PASSED
h5jam -u u10.txt -i twithub513.h5 -o tax6.h5                                  
echo h5jam -u u10.txt -i twithub513.h5 -o tax6.h5                             PASSED           
h5jam -u u511.txt -i twithub513.h5 -o tax7.h5                                 
echo h5jam -u u511.txt -i twithub513.h5 -o tax7.h5                            PASSED 
h5jam -u u512.txt -i twithub513.h5 -o tax8.h5                                 
echo h5jam -u u512.txt -i twithub513.h5 -o tax8.h5                            PASSED
h5jam -u u513.txt -i twithub513.h5 -o tax9.h5                                 
echo h5jam -u u513.txt -i twithub513.h5 -o tax9.h5                            PASSED
h5jam -u u10.txt -i twithub.h5 -o taz2.h5 --clobber                           
echo h5jam -u u10.txt -i twithub.h5 -o taz2.h5 --clobber                      PASSED
h5jam -u u511.txt -i twithub.h5 -o taz3.h5 --clobber                          
echo h5jam -u u511.txt -i twithub.h5 -o taz3.h5 --clobber                     PASSED
h5jam -u u512.txt -i twithub.h5 -o taz4.h5 --clobber                          
echo h5jam -u u512.txt -i twithub.h5 -o taz4.h5 --clobber                     PASSED
h5jam -u u513.txt -i twithub.h5 -o taz5.h5 --clobber                          
echo h5jam -u u513.txt -i twithub.h5 -o taz5.h5 --clobber                     PASSED
h5jam -u u10.txt -i twithub513.h5 -o taz6.h5 --clobber                        
echo h5jam -u u10.txt -i twithub513.h5 -o taz6.h5 --clobber                   PASSED
h5jam -u u511.txt -i twithub513.h5 -o taz7.h5 --clobber                       
echo h5jam -u u511.txt -i twithub513.h5 -o taz7.h5 --clobber                  PASSED
h5jam -u u512.txt -i twithub513.h5 -o taz8.h5 --clobber                       
echo h5jam -u u512.txt -i twithub513.h5 -o taz8.h5 --clobber                  PASSED
h5jam -u u513.txt -i twithub513.h5 -o taz9.h5 --clobber                       
echo h5jam -u u513.txt -i twithub513.h5 -o taz9.h5 --clobber                  PASSED
copy /Y twithub.h5 tay2.h5 >temp.txt   
h5jam -u u10.txt -i tay2.h5 --clobber                                         
echo h5jam -u u10.txt -i tay2.h5 --clobber                                    PASSED
copy /Y twithub.h5 tay3.h5 >temp.txt                          
h5jam -u u511.txt -i tay3.h5 --clobber                                        
echo h5jam -u u511.txt -i tay3.h5 --clobber                                   PASSED
copy /Y twithub.h5 tay4.h5 >temp.txt                        
h5jam -u u512.txt -i tay4.h5 --clobber                                        
echo h5jam -u u512.txt -i tay4.h5 --clobber                                   PASSED
copy /Y twithub.h5 tay5.h5 >temp.txt                        
h5jam -u u513.txt -i tay5.h5 --clobber                                        
echo h5jam -u u513.txt -i tay5.h5 --clobber                                   PASSED
copy /Y twithub513.h5 tay6.h5 >temp.txt                       
h5jam -u u10.txt -i tay6.h5 --clobber                                         
echo h5jam -u u10.txt -i tay6.h5 --clobber                                    PASSED
copy /Y twithub513.h5 tay7.h5 >temp.txt                           
h5jam -u u511.txt -i tay7.h5 --clobber                                        
echo h5jam -u u511.txt -i tay7.h5 --clobber                                   PASSED
copy /Y twithub513.h5 tay8.h5 >temp.txt                         
h5jam -u u512.txt -i tay8.h5 --clobber                                        
echo h5jam -u u512.txt -i tay8.h5 --clobber                                   PASSED
copy /Y twithub513.h5 tay9.h5 >temp.txt                         
h5jam -u u513.txt -i tay9.h5 --clobber                                        
echo h5jam -u u513.txt -i tay9.h5 --clobber                                   PASSED                         
del temp.txt
cd ../..
