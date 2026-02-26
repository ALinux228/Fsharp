open System

// Создание списка из n одинаковых элементов
let rec dub_list len element =
    if len <= 0 then []
    else element :: dub_list (len - 1) element 

// Вычисление произведения цифр числа рекурсивно
let rec mul_digit n =
    if n = 0 then
        1
    else
        (n % 10) * mul_digit (n / 10)

// Вставка элемента в список на указанную позицию
let rec insert pos elem list =
    if list = [] then
        // Пустой список
        if pos = 0 then
            [elem]
        else
            failwith "Ошибка: выход за диапазон допустимых значений"
    else
        // Непустой список
        let head = list.Head
        let tail = list.Tail
        
        if pos = 0 then
            elem :: head :: tail
        else
            head :: insert (pos - 1) elem tail

// Функция для создания списка вводом с клавиатуры 
let create_list() =
    printfn "Введите длину списка:" 
    let length = int(Console.ReadLine()) 
    let rec rec_create pos list =
        if pos < length then
            printf "Введите элемент %d: " (pos + 1)
            let element = int(Console.ReadLine())
            
            // Вставляем элемент
            let newList = insert pos element list
            
            // Рекурсивно продолжаем для следующей позиции
            rec_create (pos + 1) newList
        else
            list
    
    // Начинаем с пустого списка и позиции 0
    rec_create 0 []

// Удаление элемента из списка на указанной позиции
let rec remove pos list =
    if list = [] then
        // Пустой список
        failwith "Ошибка: список пуст, нельзя удалить элемент"
    else
        // Непустой список
        let head = list.Head
        let tail = list.Tail
        
        if pos = 0 then
            // Удаляем первый элемент - возвращаем хвост
            tail
        else
            // Сохраняем голову и рекурсивно удаляем из хвоста с позицией pos-1
            head :: remove (pos - 1) tail

// Получение элемента из списка по указанной позиции
let rec get pos list =
    if list = [] then
        // Пустой список
        failwith "Ошибка: список пуст"
    else
        // Непустой список
        let head = list.Head
        let tail = list.Tail
        
        if pos = 0 then
            head  // Возвращаем первый элемент
        else
            get (pos - 1) tail  // Ищем дальше

// Поиск первой позиции элемента в списке по значению
let rec find value list =
    if list = [] then
        // Пустой список - элемент не найден
        failwith "Элемент не найден"
    else
        // Непустой список
        let head = list.Head
        let tail = list.Tail
        
        if head = value then
            0  // Нашли - возвращаем позицию 0
        else
            // Ищем дальше, прибавляем 1 к результату
            let pos = find value tail
            pos + 1

// Сцепка двух списков
let rec concat list1 list2 =
    if list1 = [] then
        // Первый список пуст - возвращаем второй список
        list2
    else
        // Первый список не пуст
        let head = list1.Head
        let tail = list1.Tail
        
        // Сохраняем голову первого списка и рекурсивно сцепляем хвост с list2
        head :: concat tail list2

// Точка входа
[<EntryPoint>]
let main argv =
    printfn "Выберите действие:"
    printfn "1. Создать список из n одинаковых элементов"
    printfn "2. Вычислить произведение цифр числа" 
    printfn "3. Работа со списком (вставка)"  
    printfn "4. Сцепить два списка" 
    printfn "5. Создать список с помощью вставки"
    printfn "6. Удалить элемент"
    printfn "7. Найти элемент по позиции"
    printfn "8. Найти элемент по значению"
    printfn "0. Выход"
    
    let ch = int(Console.ReadLine())
    printfn ""

    match ch with
    | 1 ->
        printf "Введите элемент: "
        let element = int(Console.ReadLine())
        printf "Введите количество элементов: "
        let len = int(Console.ReadLine())
        let list = dub_list len element
        printfn "Созданный список: %A" list


    | 2 ->
        printf "Введите число: "  
        let number = int(Console.ReadLine())

        if number <> 0 then
            let result = mul_digit number
            printfn "Произведение цифр числа: %d" result
        else 
            printfn "Произведение цифр числа: 0"

    | 3 ->
        let list = create_list()  
        printfn "Исходный список: %A" list
        printf "Введите позицию для вставки: "
        let pos = int(Console.ReadLine())
        printf "Введите элемент для вставки: "
        let elem = int(Console.ReadLine())
        let new_list = insert pos elem list
        printfn "Список после вставки: %A" new_list

    | 4 ->
        printfn "Создание первого списка:"
        let list1 = create_list()
        printfn "Создание второго списка:"
        let list2 = create_list()
        let result = concat list1 list2
        printfn "Результат сцепки: %A" result

    | 5 ->
        let list = create_list()
        printfn "Созданный список: %A" list

    | 6 ->
        let list = create_list()
        printfn "Исходный список: %A" list
        printf "Введите позицию для удаления: "
        let pos = int(Console.ReadLine())
        let new_list = remove pos list
        printfn "Список после удаления: %A" new_list
        

    | 7 ->
        let list = create_list()
        printfn "Исходный список: %A" list
        printf "Введите позицию для получения элемента: "
        let pos = int(Console.ReadLine())
        let element = get pos list
        printfn "Элемент на позиции %d: %d" pos element

    | 8 ->
        let list = create_list()
        printfn "Исходный список: %A" list
        printf "Введите значение для поиска: "
        let value = int(Console.ReadLine())
        let pos = find value list
        printfn "Элемент %d найден на позиции: %d" value pos
    | 0 ->
        printfn "Программа завершена."

    | _ ->
        printfn "Неверный номер операции!"
    
    0