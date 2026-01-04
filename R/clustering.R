
#' @title 통합 클러스터링 및 시각화 함수
#' @description 지정된 방법과 군집 수(k)로 클러스터링 및 시각화를 수행합니다.
#' @param data 수치형 데이터프레임 또는 행렬.
#' @param k 원하는 군집의 개수.
#' @param method 사용할 알고리즘. "kmeans", "hierarchical", "tsne", "umap" 중 선택.
#' @return ggplot 시각화 객체.
#' 
#' @export perform_and_visualize_clustering
perform_and_visualize_clustering <- function(data, k, method = "umap") {

  # 1. 필수 패키지 설치 및 로드
  # ------------------------------------
  # install.packages(c("Rtsne", "umap", "ggplot2", "factoextra"))
  library(Rtsne)
  library(umap)
  library(ggplot2)
  library(factoextra)

  
    
  # 입력값 유효성 검사
  stopifnot(method %in% c("kmeans", "hierarchical", "tsne", "umap"))
  
  set.seed(123) # 결과 재현을 위한 시드 설정
  
  # 선택된 방법에 따라 분기
  switch(method,
         "kmeans" = {
           # K-means 클러스터링 직접 수행
           km_res <- kmeans(data, centers = k, nstart = 25)
           fit.cluster <- km_res
           
           # fviz_cluster를 이용한 시각화 (PCA 기반)
           p <- fviz_cluster(km_res, data = data,
                             main = paste("K-means Clustering (k =", k, ")"))
         },
         
         "hierarchical" = {
           # 계층적 클러스터링 수행
           hc_res <- hclust(dist(data))
           fit.cluster <- hc_res
           
           # fviz_cluster를 이용한 시각화
           p <- fviz_cluster(list(data = data, cluster = cutree(hc_res, k)),
                             main = paste("Hierarchical Clustering (k =", k, ")"))
         },
         
         "tsne" = {
           # t-SNE로 2차원 축소
           tsne_res <- Rtsne(data, dims = 2, perplexity = 30, check_duplicates = FALSE)
           coords <- as.data.frame(tsne_res$Y)
           colnames(coords) <- c("TSNE1", "TSNE2")
           
           # 축소된 데이터에 k-means 적용
           km_res <- kmeans(coords, centers = k, nstart = 25)
           coords$cluster <- as.factor(km_res$cluster)
           
           fit.cluster <- km_res
           
           # ggplot으로 시각화
           p <- ggplot(coords, aes(x = TSNE1, y = TSNE2, color = cluster)) +
             geom_point(alpha = 0.8) +
             labs(title = paste("t-SNE Projection with K-means Clustering (k =", k, ")")) +
             theme_minimal()
         },
         
         "umap" = {
           # UMAP으로 2차원 축소
           umap_res <- umap(data, n_components = 2, n_neighbors = 15, min_dist = 0.1)
           coords <- as.data.frame(umap_res$layout)
           colnames(coords) <- c("UMAP1", "UMAP2")
           
           # 축소된 데이터에 k-means 적용
           km_res <- kmeans(coords, centers = k, nstart = 25)
           coords$cluster <- as.factor(km_res$cluster)
           
           fit.cluster <- km_res
           
           # ggplot으로 시각화
           p <- ggplot(coords, aes(x = UMAP1, y = UMAP2, color = cluster)) +
             geom_point(alpha = 0.8) +
             labs(title = paste("UMAP Projection with K-means Clustering (k =", k, ")")) +
             theme_minimal()
         }
  )
  
  return(list(plot=p, fit.cluster=fit.cluster))
}











#' @title 최적의 k값 탐색 통합 함수
#' @description 지정된 방법으로 k-means의 최적 군집 수를 찾기 위한 시각화를 생성합니다.
#' @param data 수치형 데이터프레임 또는 행렬.
#' @param max_k 확인할 최대 군집 수.
#' @param method 평가 방법. "elbow", "silhouette", "gap_stat" 중 선택.
#' @return ggplot 시각화 객체.
#' @export find_optimal_k
find_optimal_k <- function(data, max_k = 10, method = "elbow") {
  
  # 1. 필수 패키지 설치 및 로드
  # ------------------------------------
  # install.packages("factoextra")
  library(factoextra)
  
  
  
  # 입력값 유효성 검사
  stopifnot(method %in% c("elbow", "silhouette", "gap_stat"))
  
  # factoextra의 fviz_nbclust 함수를 사용하여 시각화
  # method 인수에 따라 내부적으로 계산 방식이 달라짐
  # 'wss'는 엘보우 방법을 의미 (Total Within Sum of Squares)
  plot_method <- ifelse(method == "elbow", "wss", method)
  
  p <- fviz_nbclust(data, 
                    FUNcluster = kmeans,      # 사용할 클러스터링 알고리즘
                    method = plot_method,     # 평가 방법 선택
                    k.max = max_k) +          # 최대 k값
    labs(title = paste("Optimal number of clusters using", method, "method"))
  
  return(p)
}