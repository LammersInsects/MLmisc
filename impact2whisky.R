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
