![](https://fsharp.org/img/logo/fsharp256.png)
# F#  
F# — это мультипарадигмальный язык программирования из семейства языков .NET Framework, поддерживающий функциональное программирование помимо императивного и объектно-ориентированного.


Является достойным наследником традиций семейства языков ML. F# поддерживается компанией Microsoft как один из основных .NET языков в их флагманском продукте для разработчиков — Visual Studio 2010.


Он сочетает в себе выразительность функциональных языков, таких как OCaml и Haskell с возможностями и объектной моделью .NET.

Понятен и максимально прост. Именно так позиционировал этот язык его создатель, Дон Сайм, хотя стоит сделать оговорку, что это касается только функционального программирования, для решения привычных задач тот же C# может оказаться эффективнее.


### Назначение
Как и другие функциональные языки, F# нацелен на упрощение работы с большими данными и многопоточность, то есть востребован в науке, в создании искусственного интеллекта, там, где используются динамические базы данных, а также для улучшенного использования ресурсов многоядерных процессоров. Иначе говоря, F# можно использовать практически везде, будь на то ваша воля.

## Начало работы
Существует несколько руководств по установке и использованию F# на компьютере:
* [Visual Studio](https://docs.microsoft.com/ru-ru/dotnet/fsharp/get-started/get-started-visual-studio)
* [Visual Studio Code](https://docs.microsoft.com/ru-ru/dotnet/fsharp/get-started/get-started-vscode)
* [.NET Core CLI](https://docs.microsoft.com/ru-ru/dotnet/fsharp/get-started/get-started-command-line)

Файлы, содержащие код на F#, обычно имеют следующие расширения:

* .fs — обычный файл с кодом, который может быть скомпилирован;
* .fsi — файл описания публичного интерфейса модуля. Обычно генерируется компилятором на основе кода, а затем редактируется вручную;
* .fsx — исполняемый скрипт. Может быть запущен прямо из Windows Explorer при помощи соответствующего пункта всплывающего меню или передан на исполнение в интерактивную консоль fsi.exe.

В некоторых источниках можно встретить в начале F# кода директиву #light on. Эта директива отключает режим совместимости синтаксиса с OCaml, делая отступы в коде значимыми (как, например в Python или Haskell). В последних версиях облегчённый режим включен по умолчанию, поэтому необходимости в указании директивы #light больше нет.

## Синтаксис
Определить переменную:

    let sampleInteger = 176
Определить функцию:

    let func1 x = x*x + 3  
Функции могут быть вложенными.
Функция всегда возвращает результат последнего вычисления.

    let add3ints x y z = 
    let add2ints x y = 
    x + y
    add2ints x (add2ints y z)
    
    System.Console.WriteLine(add3ints 1 2 3)
    System.Console.ReadLine()

#### Строгая типизация
В F# нет неявного преобразования между типами.
Конвертация типов:

    let int10 = int 10.0
    let float10 = float 10
Тип функции и тип аргумента:

    let f1 (x:int) :float = float x
Приведение типов:

:> — приведение к предку

:?> — приведение к потомку

#### Операции со списками
Создание списка:

    let mylist = [1; 2; 3; 4; 5]
Получение нового списка путем преобразования старого:

    let multipliedList = List.map (fun x -> x * 3) mylist
Фильтрация списка:

    let biglist = List.filter (fun x -> x > 7) multipliedList
Оператор |> позволяет использовать функции в Linq стиле. Он передает выражение слева последним аргументом в выражение справа. На примере будет понятнее:

    let biglist2 = 
    [1; 2; 3; 4; 5]
    |> List.map (fun x -> x * 3)
    |> List.filter (fun x -> x > 7)
Конкатенация списков:

    let newlist = [1] @ [4..5]
Сворачивание списков:

    let newlist = [1 .. 3]
    let sumofsquares = 
    newlist
    |> List.reduce (fun acc x -> acc + x * x)
Списки являются однонаправленными, т.е. получение первого элемента намного быстрее, чем получение последнего. Если вам нужен последний элемент списка — скорее всего нужно пересмотреть алгоритм.

#### Рекурсия
Для того, чтобы функция была рекурсивной, нужно в объявление функции добавить ключевое слово rec

    let rec fib n = 
    if (n < 3)
    then
    1
    else
    fib(n-1) + fib(n-2)
F# оптимизирует хвостовую рекурсию. Нужно стараться делать рекурсию хвостовой.
Оптимизация хвостовой рекурсии по умолчанию отключена при сборке в Debug.

#### Pattern Matching
Сопоставление с образцом. Выражение по очереди подставляется в каждый из вариантов. Если есть совпадение – возвращается соответствующий результат.

    let rec fibn =
    match n with
    | 0 -> 0
    | 1 -> 1
    | _ ->fib (n - 1) + fib (n - 2)
Для образцов можно задавать условные ограничения:

    let sign x = 
    match x with
    | 0 -> 0
    | x when x < 0-> -1
    | x -> 1

#### Каррирование
Все функции в F# каррированы.
Рассмотрим функцию:

     let add x y = x + y
ее тип x:int ->y:int ->int
Функциям в F# не обязательно передавать все аргументы сразу. Аргументы они принимают по очереди. Например:

    let add3 = add 3
Тип этой функции int ->int

    System.Console.WriteLine(add3 4); // выведет 7

### Особенности F#
*Статически типизированный.* F# является статически типизированным языком, в отличие, например, от динамически типизированного Ruby. Это означает, что информация о типах известна во время компиляции. Благодаря этому код исполняется намного быстрее и есть возможность узнавать о большинстве ошибок еще на этапе компиляции.

*Лаконичный.* В дополнение к выведению типов, F# также использует значимые пробелы. Если сместить строки под if-конструкцией на несколько пробелов вправо, компилятор расценит их как тело оператора (light синтаксис). Это избавляет от необходимости засорять код фигурными скобками и сохраняет вертикальное пространство. Меньше кода на странице означает возможность одновременно видеть большую часть программы, и меньше переключать контекст просматривая код.

*Расширяемый.* Это означает возможность использования F# в крупных проектах уровня предприятий

*Исследуемый.* В F# вам нет необходимости обращаться к дизайну, чтобы понять, как все работает. Можно быстро прототипировать решения F#, а также экспериментировать с алгоритмами и подходами с помощью F# Interactive Window. Опираясь на лаконичную природу F#, не нужно писать много кода, чтобы экспериментировать с программами.

*Библиотеки.* F# поставляется со своей собственной библиотекой для написания функционального или другого кода[16]. Платформа .NET позволяет использовать множество уже существующих библиотек.

В языке используются такие функциональные инструменты как *каррирование* (определение функции нескольких аргументов как функции высшего порядка), функциональные типы, кортежи, связывание имен, цитирования (метапредставление абстрактного синтаксического дерева программы), вычислительные выражения (computation expressions).

Microsoft интегрировала среду разработки F# в Visual Studio 2010 и планирует активно внедрять данный язык в разработку программных систем, которые сами с течением времени смогут масштабироваться, например в зависимости от количества пользователей, данное достоинство нельзя просто реализовать в императивных языках программирования.

### Недостатки
Впрочем, F# - не сладкая вата, а живой язык со своими недостатками. Вот парочка из них:

- Слабая поддержка мобильных платформ. Для разработчиков Android и iOS приложений в последнее время есть определенные позитивные изменения, но применять F# для этих целей по-прежнему неудобно;

- Малочисленное сообщество разработчиков. Говоря языком цифр, их примерно в 100 раз меньше, чем программистов на C#.

### Области применения F#:

* Симуляция.
* Вычислительные финансы.
* Обработка крупномасштабных данных.
* Языково-ориентированное программирование.
* Написание анализаторов кода.
* Расширяемый F# код.
* Многопоточное программирование.
* Параллельное программирование.
* Асинхронное программирование.

Примером использования F# является коммерческий продукт WebSharper фирмы IntelliFactory. Это платформа для создания клиентских web-приложений. Она позволяет писать клиентский код на F#, который затем будет оттранслирован на JavaScript. Такая трансляция распространяется на достаточное большое подмножество F#, включая функциональное ядро языка, алгебраические типы данных, классы, объекты, исключения и делегаты. Также поддерживается значительная часть стандартной библиотеки F#, включая работу с последовательностями (sequences), событиями и асинхронными вычислительными выражениями (async workflows). Всё может быть автоматически оттранслировано на целевой язык JavaScript. Кроме того, поддерживается некоторая часть стандартных классов самого .NET, и объём поддержки будет расти 

### Чем различаются C# и F#
Оба языка разрабатываются компанией Microsoft, одинаково компилируются и поддерживаются одними и теми же платформами. Даже производительность у них практически на одном уровне. Однако они всё равно достаточно сильно различаются.

Главное различие этих языков — в их парадигмах. Парадигма программирования — это набор идей и понятий, которые определяют стиль написания приложений.

В C# используется императивная парадигма. В ней программист пишет инструкции для компьютера, то есть отдает ему приказы. Чтобы делать это было удобнее, используется  объектно-ориентированное программирование.

При этом выполненная инструкция может вернуть какой-нибудь результат, а может этого и не делать. Например, выполнив операцию сложения, компьютер вернет сумму чисел. А если попросить его вывести надпись, то он просто выведет ее.

    var x = 5 + 5; //Программа выполнит команду сложения и сохранит в переменную x сумму двух чисел
    Console.WriteLine(x); //Программа обращается к объекту Консоль, чтобы он вывел значение переменной x. Эта инструкция выполняется, но никакое значение не сохраняется

При этом важен порядок действий, так как подразумевается сохранение состояния программы. То есть если во время вычислений появляются какие-то промежуточные значения, то их обязательно нужно где-то хранить.

В F# разработчик использует декларативную парадигму и функциональное программирование. В F# вместо инструкций — выражения, которые всегда возвращают какое-то значение. Разработчик не отдает приказы, а описывает проблему, способ ее решения (функцию) и ожидаемый результат. То есть нужно сказать программе, что делать, а решит задачу она сама.

**C#**

    int Fac(int n)
    {
	    if(n < 2)
	    {
		    return 1;
	    }
	    else
	    {
		    return n * Fac(n - 1);
	    }
    }
**F#**

    let rec fac n =
    if n < 2 then 1
    else n * fac(n - 1)

В примере выше показано получение факториала в C# и F#. В обоих фрагментах кода есть конструкция if-else, которая позволяет изменять ход выполнения программы в зависимости от того, было ли соблюдено условие. В C# с помощью этой конструкции указываются какие-то действия (в данном случае используется оператор return, который возвращает значение), а в F# — значения, которые будут возвращены.

#### Использование кода на F# из C#
Создаем F# Library, в созданном файле пишем такой код:

    namespace Library1
    module IntFunctions=
    let private add_ints x y = x + y
    let addints x y = add_ints x y
Здесь объявлены 2 функции: одна приватная и одна публичная.
Подключаем эту библиотеку к проекту на C# и используем:

    using System;
    using Library1;
    
    namespace ConsoleApplication22
    {
  	classProgram
  	{
    		staticvoid Main(string[] args)
    		{
     			var q = IntFunctions.addints(3, 4);
      			Console.WriteLine(q.ToString());
      			Console.ReadLine();
    		}
  	}
	}
Если перейти к определению класса IntFunctions, то увидим следующее:

    using System;
    
    namespace Library1
    {
  	public static class IntFunctions
  	{
    		public static int addints(int x, int y);
  	}
	}
Модуль виден как статический класс, функция видна как статический метод.

**Сайт языка**: [F#](https://fsharp.org)

* [Документация](https://fsharp.org/about/index.html#documentation)
* [Русскоязычное сообщество](http://fsharplang.ru/)
* [Все для изучения](https://fsharp.org/learn/index.html)
* [Курсы](https://www.udemy.com/topic/f-sharp/?persist_locale&locale=ru_RU)
* [Хабр F# Введение](https://habr.com/ru/post/116666/)
