

# 呼叫core-nlp api
call_coreNLP <- function(server_host, text, host="localhost", language="eng",
                         tokenize.whitespace="FALSE", ssplit.eolonly="true", annotators=c("tokenize","ssplit","pos","lemma","ner","parse","sentiment")){
  # 假設有兩個core-nlp server、一個負責英文（使用9000 port）、另一個則負責中文（使用9001 port）
  port <- ifelse(language=="eng", 9000, 9001);
  # 產生api網址
  url <- generate_API_url(server_host, port=port,
                          tokenize.whitespace=tokenize.whitespace, annotators=paste0(annotators, collapse = ','))
  
  result <- POST(url, body = text, encode = "json")
  doc <- httr::content(result, "parsed","application/json",encoding = "UTF-8")
  return (doc)
}


coreNLP <- function(data,host){
  # 依序將每個文件丟進core-nlp進行處理，每份文件的回傳結果為json格式
  # 在R中使用objects來儲存處理結果
  result <- apply(data, 1 , function(x){
    object <- call_coreNLP(host, x['text'])
    list(doc=object, data=x)
  })
  
  return(result)
}



coreNLP_tokens_parser <- function(coreNLP_objects){
  
  result <- do.call(rbind, lapply(coreNLP_objects, function(obj){
    original_data <- obj$data
    doc <- obj$doc
    # for a sentences
    sentences <- doc$sentences
    
    sen <- sentences[[1]]
    
    tokens <- do.call(rbind, lapply(sen$tokens, function(x){
      result <- data.frame(word=x$word, lemma=x$lemma, pos=x$pos, ner=x$ner)
      result
    }))
    
    tokens <- original_data %>%
      t() %>% 
      data.frame() %>% 
      select(-text) %>% 
      slice(rep(1:n(), each = nrow(tokens))) %>% 
      bind_cols(tokens)
    
    tokens
  }))
  return(result)
}

coreNLP_dependency_parser <- function(coreNLP_objects){
  result <- do.call(rbind, lapply(coreNLP_objects, function(obj){
    original_data <- obj$data
    doc <- obj$doc
    # for a sentences
    sentences <- doc$sentences
    sen <- sentences[[1]]
    dependencies <- do.call(rbind, lapply(sen$basicDependencies, function(x){
      result <- data.frame(dep=x$dep, governor=x$governor, governorGloss=x$governorGloss, dependent=x$dependent, dependentGloss=x$dependentGloss)
      result
    }))
    
    dependencies <- original_data %>%
      t() %>% 
      data.frame() %>% 
      select(-text) %>% 
      slice(rep(1:n(), each = nrow(dependencies))) %>% 
      bind_cols(dependencies)
    dependencies
  }))
  return(result)
}



coreNLP_sentiment_parser <- function(coreNLP_objects){
  result <- do.call(rbind, lapply(coreNLP_objects, function(obj){
    original_data <- obj$data
    doc <- obj$doc
    # for a sentences
    sentences <- doc$sentences
    sen <- sentences[[1]]
    
    sentiment <- original_data %>%
      t() %>% 
      data.frame() %>% 
      bind_cols(data.frame(sentiment=sen$sentiment, sentimentValue=sen$sentimentValue))
    
    sentiment
  }))
  return(result)
}




# 圖形化顯示dependency結果
parse2tree <- function(ptext) {
  stopifnot(require(NLP) && require(igraph))
  
  # this step modifies coreNLP parse tree to mimic openNLP parse tree
  ptext <- gsub("[\r\n]", "", ptext)
  ptext <- gsub("ROOT", "TOP", ptext)
  
  
  ## Replace words with unique versions
  ms <- gregexpr("[^() ]+", ptext)                                      # just ignoring spaces and brackets?
  words <- regmatches(ptext, ms)[[1]]                                   # just words
  regmatches(ptext, ms) <- list(paste0(words, seq.int(length(words))))  # add id to words
  
  ## Going to construct an edgelist and pass that to igraph
  ## allocate here since we know the size (number of nodes - 1) and -1 more to exclude 'TOP'
  edgelist <- matrix('', nrow=length(words)-2, ncol=2)
  
  ## Function to fill in edgelist in place
  edgemaker <- (function() {
    i <- 0                                       # row counter
    g <- function(node) {                        # the recursive function
      if (inherits(node, "Tree")) {            # only recurse subtrees
        if ((val <- node$value) != 'TOP1') { # skip 'TOP' node (added '1' above)
          for (child in node$children) {
            childval <- if(inherits(child, "Tree")) child$value else child
            i <<- i+1
            edgelist[i,1:2] <<- c(val, childval)
          }
        }
        invisible(lapply(node$children, g))
      }
    }
  })()
  
  ## Create the edgelist from the parse tree
  edgemaker(Tree_parse(ptext))
  tree <- FromDataFrameNetwork(as.data.frame(edgelist))
  return (tree)
}


