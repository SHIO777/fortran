! compile as 
! f95 ex2_2.f90 XCgraphics.o -o ex2_2.x -lX11

program x_y
    implicit none
    integer i
    integer, parameter :: num = 20, np = 100
    character a*1
    character, parameter :: x = "x", y = "y"
    real, parameter :: width = 500.0, height = 500.0, m = 20.0    ! screen size, m = margin
    real y_margin, x_margin
    
    call xcinit(width, height)
    call xccolor(2)
    call xcline(20.0, height/2.0, width-20.0, height/2.0)   ! x軸描画
    call xcline(width/2.0, 20.0, width/2.0, height-20.0)    ! y軸描画
    call xcstring(width-20.0, height/2.0+10.0, x, 1)        ! "x"を表示
    call xcstring(width/2.0+10.0, 20.0, y, 1)               ! "y"を表示
    call xcstring(width/2.0-10.0, height/2.0+15.0, "0", 1)  ! 原点に"0"を表示

    ! x軸矢印描画
    call xcline(width-20.0, height/2.0, width-30.0, height/2.0+3.0)
    call xcline(width-20.0, height/2.0, width-30.0, height/2.0-3.0)
    ! y軸矢印描画
    call xcline(width/2.0, 20.0, width/2.0+3.0, 20.0+10.0)
    call xcline(width/2.0, 20.0, width/2.0-3.0, 20.0+10.0)

    ! 格子作成
    x_margin = width - m * 2
    y_margin = height - m * 2
    do i = 0, num
        call dashed_line(m, y_margin*i/num+m, width-m, y_margin*i/num+m, np)
        call dashed_line(x_margin*i/num+m, m, x_margin*i/num+m, height-m, np)
    end do
    
    write (6,*) "press enter"
    read (5,'(a)') a
  
    call xcclose (1)

end program x_y


subroutine dashed_line(x1, y1, x2, y2, np)
    ! =======================
    ! 破線を描画するサブルーチーン
    ! x1, y1 - 始点(x1, y1)
    ! x2, y2 - 終点(x2, y2)
    ! np - 分割数
    ! =======================
    implicit none
    real, dimension(np + 1) :: xpoint, ypoint
    real, intent(in) :: x1, y1, x2, y2
    real delta_x, delta_y
    integer, intent(in) :: np
    integer i

    delta_x = x2 - x1       ! x方向の差分を計算
    delta_y = y2 - y1       ! y方向の差分を計算

    do i = 0, np
        ! np個に分割し，各地点をxpoint, ypointという配列に格納
        xpoint(i + 1) = x1 + delta_x * i / np
        ypoint(i + 1) = y1 + delta_y * i / np
        ! print *, xpoint(i+1), ypoint(i+1)
    end do

    do i = 1, np, 2
        ! 各地点と地点を2つおきに線で結ぶ
        call xcline(xpoint(i), ypoint(i), xpoint(i + 1), ypoint(i + 1))
    end do 

end subroutine dashed_line