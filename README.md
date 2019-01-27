# Location-prediction
Predicted geo-location of 80,000 tweets based on just its contents by finding Location Indicative Words and achieved 74% accuracy 

#DataSet
Used the dataset http://followthehashtag.com/datasets/170000-uk-geolocated-tweets-free-twitter-dataset/
The dataset obtained contained about 170,000 geotagged tweets most of which were in several languages from users based in UK, France, Ireland and Isle of Man. The dimensions of the obtained dataset were approximately 170,000 by 19. Other information included the user profile,time at which the tweet was posted, the tweet URL, favorites and retweets each tweet got.Initially, subsets of tweets were created where only the tweet content along with its geographical coordinates were taken. For the project, tweets written by users based in England and tweets that are written only in English language have been considered.

#Reverse Geocoding

The dataset had locations in the form of latitude and longitude, across multiple countries and therefore, the major task was to reverse- geocode all the latitude-longitude points. For this task, a dataset was manually created which contained all the cities in England along with the latitude and longitude of the central point of the city. The dataset also contained an approximate radius for each city. In order to reverse geocode the geographical points in the dataset, following approach was used.For every point j in the dataset, distance was calculated with the central point of each of the cities. If the calculated distance was less than the radius of the city i, point j was classified as being in the city. For example, distance was calculated for every geographical point given in the dataset with the central point of a city, say, London. If the distance for any of the geographical point in the dataset was less than the radius of London, the geographical point was considered to be in the city and therefore, classified as “London”. This approach was applied for all the 50 cities in England. The points that were not in any of the city, that is, points outside England, were classified as “Not in England”. After obtaining the final classified dataset, tweets classified as “Not in England” were removed from the dataset. Therefore, the final dataset obtained contained about 70,000 tweets.

#Cleaning the dataset

As the project is about prediction based on the contents of the tweet, therefore, it is essential to first clean the contents of the tweets. There were several tweets that contained hyperlinks in them. Therefore, all the hyperlinks that began with “https://” or “www” were removed from the tweets. Then, the tweets were processed through several basic cleaning steps. This involved lowercasing all characters removing all punctuations and stop-words from the tweets. A large list of stopwords was considered for the project. The list had about 667 stop-words. Also, all the mentions were removed from tweets, that is, words that begin with “@” were removed from all the tweets.
Other cleaning steps involved converting all special characters such as accented characters and letters to ASCII characters. Finally, all the graphical characters were removed from the tweets.



#Finding Location Indicative Words

In order to classify each of the tweets, the basic approach employed is to find location indicative words. Many places around the world have few slangs or words that are specific to the city or a region. For example, the word “Knicks” would be more frequently used in a place like New York rather than a place such as New Delhi, India. In order to find such words, each word was weighed with their TF-IDF scores.

#Classification

For classification, the final dataset was divided into a training and testing dataset with 70% of tweets in the training data and remaining 30% in the testing set. Several techniques were employed in order to train the model. Initially, Naïve Bayes algorithm was employed which gave abnormally bad results. Other techniques employed were Decision Trees, SVM and Nearest Neighbors. While classifying, the parameters given to the functions were cross-validated.
