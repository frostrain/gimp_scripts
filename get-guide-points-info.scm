;; 返回 所有辅助线的id 的列表
(define (get-gids image-id)
  (define gids '())
  (define current-id 0)
  (define (add-next-id current-id)
    ;; 注意 next-id 是一个 list
    (let ((next-id (gimp-image-find-next-guide image-id current-id)))
      (if (equal? (car next-id) 0)
          gids
          (begin (set! gids (append gids next-id))
                 (add-next-id (car next-id))))))
  (add-next-id current-id))

;; 返回所有辅助线 交点 的坐标列表, 每个元素都是 (x y)
;; 注意 gimp 的脚本中没有原生的 sort 函数
(define (get-guide-points image-id gids)
  (define v-guides '())
  (define h-guides '())
  (define (get-positions-by-guides horizontals verticals)
    (define positions '())
    (map (lambda (x)
           (map (lambda (y) (set! positions (cons (list x y) positions)))
                verticals))
         horizontals)
    positions)
  (map (lambda (gid)
         (let ((orient (car (gimp-image-get-guide-orientation image-id gid)))
               (position (car (gimp-image-get-guide-position image-id gid))))
           (if (equal? orient 1)
               (set! v-guides (cons position v-guides))
               (set! h-guides (cons position h-guides))))) gids)
  (get-positions-by-guides v-guides h-guides))

;; 将十进制的 数字 转化为 十六进制的字符串
;; 返回的是字符串(1位或2位字符)
(define (toHex n)
    (define (integer->string n base)
      (define digit-list
        (list->vector (string->list "0123456789abcdefghijklmnopqrstuvwxyz")))
      (define (conv n r)
        (if (zero? n) (list->string r)
            (conv (quotient n base)
                  (cons (vector-ref digit-list (remainder n base)) r))))
      (cond ((zero? n) "0")
            ((< n 0)
             (string-append "-" (conv (- n) '())))
            (else (conv n '()))))
    (integer->string n 16))

;; 将十进制的 数字颜色 转化为 十六进制的颜色字符串(两位字符长度)
;; 返回的是字符串
(define (to-hex-color n)
  (let ((hex (toHex n)))
    (if (= (string-length hex) 1)
        ;; 补齐十六进制y颜色前面的0(对于1位字符的情况)
        (string-append "0" hex)
        hex)))

;; 返回 所有辅助线交叉点的坐标和颜色信息的列表
;; 其中每个元素是这样的列表 (x y color), 也就是 x点坐标, y点坐标, 16进制颜色值
(define (get-guide-points-info image-id draw-id)
  (define points (get-guide-points image-id (get-gids image-id)))
  (define (rgb->hex rgb-list)
    (string-append "0x"
                   (to-hex-color (car rgb-list))
                   (to-hex-color (cadr rgb-list))
                   (to-hex-color (caddr rgb-list))))
  (define (get-point-info point)
    (let* ((x (car point))
           (y (cadr point))
           (color (gimp-image-pick-color image-id draw-id x y 0 0 0)))
      (list x y (rgb->hex (car color)))))
  (define infos '())
  (map (lambda (point)
         (get-point-info point)) points))

;; 使用 concatation 来连接字符串列表 string-list, 返回连接后的字符串
(define (map-concat string-list concatation)
  (define car-element (car string-list))
  (if (null? car-element)
      ""
      (if (null? (cdr string-list))
          car-element
          (string-append car-element concatation (map-concat (cdr string-list) concatation)))
      ))

;; 显示所有辅助线交叉点的坐标和颜色信息, 一般用于多点取色
;; 显示结果(字符串)类似
;; {1101, 636, 0xa59a9c },
;; {1101, 333, 0x212c42 },
;; {660, 636, 0x424142 },
;; {660, 333, 0x212c39 }
(define (display-guide-points-info image-id draw-id)
  (define points (get-guide-points-info image-id draw-id))
  (if (null? points)
      (gimp-message "没有辅助线交叉点!")
      (begin
  (define points-list (map (lambda (p)
                             (string-append "{ " (number->string (car p)) ", " (number->string (cadr p)) ", " (caddr p) " }"))
                           points))
  (gimp-message (map-concat points-list ",\n")))
      ))


;; 注册过程(procedure)
(script-fu-register
 "display-guide-points-info"                        ;func name
 "display-guide-points-info"                                  ;menu label
 "显示辅助线交点的坐标和颜色"         ;description
 "frostrain"                             ;author
 "mit"        ;copyright notice
 "2017-11-17"                          ;date created
 ""                     ;image type that the script works on
 ;; 上面七个参数是必须的
 ;; 下面的是弹出窗口的选项ui, 四个选项对应 script-fu-text-box 的四个参数
 SF-IMAGE       "Image"         0 ;; 如果需要 当前的ImageId 来进行操作, 第一个参数必须是 SF-IMAGE, gimp会自动传入当期的 ImageId
 SF-DRAWABLE     "Drawable"     0 ;; 类似 ImageId, 这里会 自动传入 DrawbleId, SF-DRAWABLE 必须是第二个参数
 )

;; 在菜单上注册
;; 第二个参数是注册的菜单位置, <Image> 是根路径
;; 注册后可以在 File -> Create -> Text 里面找到一个 Text Box 的按钮
(script-fu-menu-register "display-guide-points-info" "<Image>/Scripts")
