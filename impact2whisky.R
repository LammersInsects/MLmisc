# IMPACT 2 WHISKY v1.2
#
# Copyright (c) 2017, Mark Lammers, Vrije Universiteit Amsterdam
# All Rights Reserved.
#
# Unless required by applicable law or agreed to in writing, this
# software is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
# CONDITIONS OF ANY KIND, either express or implied.  See the License
# for the specific language governing permissions and limitations
# under the License.
#
# by: M. Lammers, 2017
#
# Conditions for whisky:
# -Only first author publications count
# -Shared first authorships count only for 75%
# x=impact factor; y=amount of euros to be spend on whiskey
# 
# # Convert impact factor to whiskey
# x <- 2.329  #Choose an impact factor
# y <- 40*log10(2*(x+1)); y  #Arbitrary conversion formula to calculate euros
# 
# # Plot the relationship between impact and whiskey
# x <- seq(0.1,30,0.1)
# y <- 40*log10(2*(x+1))  #Same formula
# plot(x,y, ylim=c(0,70), type='l')


impact2whisky<-function(IF, print.help=T){
  if(print.help){
    cat('A custom formula to convert the impact factor (IF) of a journal\n')
    cat('to a price for a bottle of Scotch single malt whisky\n')
    cat('in celebration of every first author paper\n')
    cat('usage: impact2whisky(IF)\n')
  }
  euro <- 40*log10(2*(IF+1)) #Arbitrary conversion formula
  euro <- round(euro,2)
  print(paste('Impactfactor',IF,'corresponds to',euro,'euro for a bottle of whisky'))
  print('Enjoy!')
}
