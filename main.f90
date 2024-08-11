program main
    implicit none
    
    integer :: opcion
    
    do 

        print *, "---------------------------------------------------------------"
        print *, "Practica 1 - Lenguajes Formales y de Programacion - Seccion: A-"
        print *, "---------------------------------------------------------------"
        print *, "# Sistema de Inventario"
        print *, " "
        print *, "1. Cargar Inventario Inicial"
        print *, "2. Cargar instrucciones de movimiento"
        print *, "3. Crear Informe de Inventario"
        print *, "4. Salir"
        print *, " "
        print *, "Ingrese una opcion: "
        read *, opcion
        select case (opcion)
            case(1)
                call cargarInventario()

            case(2)
                call cargarInstrucciones

            case(3)
                call crearInforme

            case(4)
                print *, "Saliendo del sistema"
                print *, "Presione Enter para salir del programa"
                read *
                call system ("cls")
                stop

            case default
                print *, "Opcion no valida"
                print *, "Presione Enter para volver al menu principal" 
                read *

        end select
        call system ("cls")
    end do

    

end program main

subroutine cargarInventario()
    print *, "Se ha cargado el inventario exitosamente"
    print *, "Presione una tecla para continuar"
    read *

end subroutine cargarInventario

subroutine cargarInstrucciones()
    print *, "Se han cargado las instrucciones exitosamente"
    print *, "Presione una tecla para continuar"
    read *

end subroutine cargarInstrucciones

subroutine crearInforme()
    print *, "Se ha creado el informe exitosamente"
    print *, "Presione una tecla para continuar"
    read *

end subroutine crearInforme