(ns simple-text-summary.core
  (require [clojure.string :as string]))

(defn split-content-to-sentences
  "Naive method for splitting a text into sentences"
  [content]
  (remove #(= "" %) (string/split content #"\n|\?|\!|\. ")))

(defn split-content-to-paragraphs
  "Naive method for splitting a text into paragraphs"
  [content]
  (string/split content #"\n\n"))

(defn sentences-intersection
  "Caculate the intersection between 2 sentences"
  [s1 s2]
  (let [s1-set (set (string/split s1 #" "))
        s2-set (set (string/split s2 #" "))
        intersect-ln (count (clojure.set/intersection s1-set s2-set))]
    ; We normalize the result by the average number of words
    (/ intersect-ln (/ (+ (count s1-set) (count s2-set)) 2))))

(defn format-sentence
  "Remove all non-alphbetic chars from the sentence"
  [sentence]
  (string/replace sentence #"\W+" ""))

(defn get-sentences-ranks
  "Convert the content into a dictionary
   {k = The formatted sentence
    v = The rank of the sentence}"
  [content]
  (let [sentences (split-content-to-sentences content)]
    (into {}
      (map
        (fn [s]
          {s
           ; The score of a sentences is the sum of all its intersection
           (reduce +
             (map (partial sentences-intersection s) (remove #{s} sentences)))})
       sentences))))

(defn get-best-sentence
  "Return the best sentence in a paragraph"
  [paragraph sentences-dic]
  (let [sentences (split-content-to-sentences paragraph)]
    ; Ignore short paragraphs
    (when (> (count sentences) 2)
      ; Get the best sentence according to the sentences dictionary
      (key (apply max-key val (select-keys sentences-dic sentences))))))

(defn get-summary
  "Build the summary"
  [title content sentences-dic]
  (let [paragraphs (split-content-to-paragraphs content)
        best-sentences (map #(get-best-sentence % sentences-dic) paragraphs)]
    (string/join "\n" (cons title best-sentences))))

(defn -main
  []
  (let [title "Swayy is a beautiful new dashboard for discovering and curating online content [Invites]"
        content "Lior Degani, the Co-Founder and head of Marketing of Swayy, pinged me last week when I was in California to tell me about his startup and give me beta access. I heard his pitch and was skeptical. I was also tired, cranky and missing my kids – so my frame of mind wasn’t the most positive.\n\n I went into Swayy to check it out, and when it asked for access to my Twitter and permission to tweet from my account, all I could think was, “If this thing spams my Twitter account I am going to bitch-slap him all over the Internet.” Fortunately that thought stayed in my head, and not out of my mouth.\n\n One week later, I’m totally addicted to Swayy and glad I said nothing about the spam (it doesn’t send out spam tweets but I liked the line too much to not use it for this article). I pinged Lior on Facebook with a request for a beta access code for TNW readers. I also asked how soon can I write about it. It’s that good. Seriously. I use every content curation service online. It really is That Good.\n\n What is Swayy? It’s like Percolate and LinkedIn recommended articles, mixed with trending keywords for the topics you find interesting, combined with an analytics dashboard that shows the trends of what you do and how people react to it. I like it for the simplicity and accuracy of the content curation. Everything I’m actually interested in reading is in one place – I don’t have to skip from another major tech blog over to Harvard Business Review then hop over to another major tech or business blog. It’s all in there. And it has saved me So Much Time\n\n After I decided that I trusted the service, I added my Facebook and LinkedIn accounts. The content just got That Much Better. I can share from the service itself, but I generally prefer reading the actual post first – so I end up sharing it from the main link, using Swayy more as a service for discovery.\n\n I’m also finding myself checking out trending keywords more often (more often than never, which is how often I do it on Twitter.com).\n\n The analytics side isn’t as interesting for me right now, but that could be due to the fact that I’ve barely been online since I came back from the US last weekend. The graphs also haven’t given me any particularly special insights as I can’t see which post got the actual feedback on the graph side (however there are numbers on the Timeline side.) This is a Beta though, and new features are being added and improved daily. I’m sure this is on the list. As they say, if you aren’t launching with something you’re embarrassed by, you’ve waited too long to launch.\n\n It was the suggested content that impressed me the most. The articles really are spot on – which is why I pinged Lior again to ask a few questions:\n\n How do you choose the articles listed on the site? Is there an algorithm involved? And is there any IP?\n\n Yes, we’re in the process of filing a patent for it. But basically the system works with a Natural Language Processing Engine. Actually, there are several parts for the content matching, but besides analyzing what topics the articles are talking about, we have machine learning algorithms that match you to the relevant suggested stuff. For example, if you shared an article about Zuck that got a good reaction from your followers, we might offer you another one about Kevin Systrom (just a simple example).\n\n Who came up with the idea for Swayy, and why? And what’s your business model?\n\n Our business model is a subscription model for extra social accounts (extra Facebook / Twitter, etc) and team collaboration.\n\n The idea was born from our day-to-day need to be active on social media, look for the best content to share with our followers, grow them, and measure what content works best.\n\n Who is on the team?\n\n Ohad Frankfurt is the CEO, Shlomi Babluki is the CTO and Oz Katz does Product and Engineering, and I [Lior Degani] do Marketing. The four of us are the founders. Oz and I were in 8200 [an elite Israeli army unit] together. Emily Engelson does Community Management and Graphic Design.\n\n If you use Percolate or read LinkedIn’s recommended posts I think you’ll love Swayy.\n\n ➤ Want to try Swayy out without having to wait? Go to this secret URL and enter the promotion code thenextweb . The first 300 people to use the code will get access.\n\n Image credit: Thinkstock"
        sentences-dic (get-sentences-ranks content)
        summary (get-summary title content sentences-dic)]
  (println summary)
  (println "Original Length" (+ (count title) (count content)))
  (println "Summary Length" (count summary))
  (println "Summary Ratio" (float (- 100 (* 100 (/ (count summary) (+ (count title) (count content)))))))))
