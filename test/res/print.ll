@fmt.i32 = constant [3 x i8] c"%d "
declare i32 @printf(i8* noalias nocapture, ...)
declare i8* @malloc(i64)
declare void @free(i8*)
define i64 @main() {
  %fun.1977 = bitcast i8* ()* @state.1110 to i8*
  %cast.1978 = bitcast i8* %fun.1977 to i8* ()*
  %arg.1739 = call i8* %cast.1978()
  %cursor.1980 = bitcast i8* (i8*, i8*)* @lam.1738 to i8*
  %sizeptr.1996 = getelementptr i64, i64* null, i32 0
  %size.1997 = ptrtoint i64* %sizeptr.1996 to i64
  %cursor.1981 = call i8* @malloc(i64 %size.1997)
  %cast.1987 = bitcast i8* %cursor.1981 to {}*
  %sizeptr.1998 = getelementptr i64, i64* null, i32 2
  %size.1999 = ptrtoint i64* %sizeptr.1998 to i64
  %ans.1979 = call i8* @malloc(i64 %size.1999)
  %cast.1982 = bitcast i8* %ans.1979 to {i8*, i8*}*
  %loader.1985 = getelementptr {i8*, i8*}, {i8*, i8*}* %cast.1982, i32 0, i32 0
  store i8* %cursor.1980, i8** %loader.1985
  %loader.1983 = getelementptr {i8*, i8*}, {i8*, i8*}* %cast.1982, i32 0, i32 1
  store i8* %cursor.1981, i8** %loader.1983
  %fun.1740 = bitcast i8* %ans.1979 to i8*
  %base.1988 = bitcast i8* %fun.1740 to i8*
  %castedBase.1989 = bitcast i8* %base.1988 to {i8*, i8*}*
  %loader.1995 = getelementptr {i8*, i8*}, {i8*, i8*}* %castedBase.1989, i32 0, i32 0
  %down.elim.cls.1741 = load i8*, i8** %loader.1995
  %loader.1994 = getelementptr {i8*, i8*}, {i8*, i8*}* %castedBase.1989, i32 0, i32 1
  %down.elim.env.1742 = load i8*, i8** %loader.1994
  %fun.1990 = bitcast i8* %down.elim.cls.1741 to i8*
  %arg.1991 = bitcast i8* %down.elim.env.1742 to i8*
  %arg.1992 = bitcast i8* %arg.1739 to i8*
  %cast.1993 = bitcast i8* %fun.1990 to i8* (i8*, i8*)*
  %tmp.2000 = tail call i8* %cast.1993(i8* %arg.1991, i8* %arg.1992)
  %cast.2001 = ptrtoint i8* %tmp.2000 to i64
  ret i64 %cast.2001
}
define i8* @lam.1671(i8* %env.1670, i8* %A.1112) {
  %base.1973 = bitcast i8* %env.1670 to i8*
  %castedBase.1974 = bitcast i8* %base.1973 to {}*
  %sizeptr.2002 = getelementptr i64, i64* null, i32 0
  %size.2003 = ptrtoint i64* %sizeptr.2002 to i64
  %ans.1975 = call i8* @malloc(i64 %size.2003)
  %cast.1976 = bitcast i8* %ans.1975 to {}*
  ret i8* %ans.1975
}
define i8* @lam.1673(i8* %env.1672, i8* %S.1111) {
  %base.1962 = bitcast i8* %env.1672 to i8*
  %castedBase.1963 = bitcast i8* %base.1962 to {}*
  %cursor.1965 = bitcast i8* (i8*, i8*)* @lam.1671 to i8*
  %sizeptr.2004 = getelementptr i64, i64* null, i32 0
  %size.2005 = ptrtoint i64* %sizeptr.2004 to i64
  %cursor.1966 = call i8* @malloc(i64 %size.2005)
  %cast.1972 = bitcast i8* %cursor.1966 to {}*
  %sizeptr.2006 = getelementptr i64, i64* null, i32 2
  %size.2007 = ptrtoint i64* %sizeptr.2006 to i64
  %ans.1964 = call i8* @malloc(i64 %size.2007)
  %cast.1967 = bitcast i8* %ans.1964 to {i8*, i8*}*
  %loader.1970 = getelementptr {i8*, i8*}, {i8*, i8*}* %cast.1967, i32 0, i32 0
  store i8* %cursor.1965, i8** %loader.1970
  %loader.1968 = getelementptr {i8*, i8*}, {i8*, i8*}* %cast.1967, i32 0, i32 1
  store i8* %cursor.1966, i8** %loader.1968
  ret i8* %ans.1964
}
define i8* @state.1110() {
  %cursor.1954 = bitcast i8* (i8*, i8*)* @lam.1673 to i8*
  %sizeptr.2008 = getelementptr i64, i64* null, i32 0
  %size.2009 = ptrtoint i64* %sizeptr.2008 to i64
  %cursor.1955 = call i8* @malloc(i64 %size.2009)
  %cast.1961 = bitcast i8* %cursor.1955 to {}*
  %sizeptr.2010 = getelementptr i64, i64* null, i32 2
  %size.2011 = ptrtoint i64* %sizeptr.2010 to i64
  %ans.1953 = call i8* @malloc(i64 %size.2011)
  %cast.1956 = bitcast i8* %ans.1953 to {i8*, i8*}*
  %loader.1959 = getelementptr {i8*, i8*}, {i8*, i8*}* %cast.1956, i32 0, i32 0
  store i8* %cursor.1954, i8** %loader.1959
  %loader.1957 = getelementptr {i8*, i8*}, {i8*, i8*}* %cast.1956, i32 0, i32 1
  store i8* %cursor.1955, i8** %loader.1957
  ret i8* %ans.1953
}
define i8* @io.1136(i8* %state.1116) {
  %sizeptr.2012 = getelementptr i64, i64* null, i32 0
  %size.2013 = ptrtoint i64* %sizeptr.2012 to i64
  %ans.1942 = call i8* @malloc(i64 %size.2013)
  %cast.1943 = bitcast i8* %ans.1942 to {}*
  %arg.1674 = bitcast i8* %ans.1942 to i8*
  %ans.1944 = bitcast i8* %state.1116 to i8*
  %fun.1675 = bitcast i8* %ans.1944 to i8*
  %base.1945 = bitcast i8* %fun.1675 to i8*
  %castedBase.1946 = bitcast i8* %base.1945 to {i8*, i8*}*
  %loader.1952 = getelementptr {i8*, i8*}, {i8*, i8*}* %castedBase.1946, i32 0, i32 0
  %down.elim.cls.1676 = load i8*, i8** %loader.1952
  %loader.1951 = getelementptr {i8*, i8*}, {i8*, i8*}* %castedBase.1946, i32 0, i32 1
  %down.elim.env.1677 = load i8*, i8** %loader.1951
  %fun.1947 = bitcast i8* %down.elim.cls.1676 to i8*
  %arg.1948 = bitcast i8* %down.elim.env.1677 to i8*
  %arg.1949 = bitcast i8* %arg.1674 to i8*
  %cast.1950 = bitcast i8* %fun.1947 to i8* (i8*, i8*)*
  %tmp.2014 = tail call i8* %cast.1950(i8* %arg.1948, i8* %arg.1949)
  ret i8* %tmp.2014
}
define i8* @lam.1682(i8* %env.1681, i8* %arg2.1680) {
  %base.1934 = bitcast i8* %env.1681 to i8*
  %castedBase.1935 = bitcast i8* %base.1934 to {i8*}*
  %loader.1941 = getelementptr {i8*}, {i8*}* %castedBase.1935, i32 0, i32 0
  %arg1.1679 = load i8*, i8** %loader.1941
  %arg.1936 = bitcast i8* %arg1.1679 to i8*
  %arg.1937 = bitcast i8* %arg2.1680 to i8*
  %cast.1938 = ptrtoint i8* %arg.1936 to i32
  %cast.1939 = ptrtoint i8* %arg.1937 to i32
  %result.1940 = mul i32 %cast.1938, %cast.1939
  %result.2015 = inttoptr i32 %result.1940 to i8*
  ret i8* %result.2015
}
define i8* @lam.1684(i8* %env.1683, i8* %arg1.1679) {
  %base.1920 = bitcast i8* %env.1683 to i8*
  %castedBase.1921 = bitcast i8* %base.1920 to {}*
  %cursor.1923 = bitcast i8* (i8*, i8*)* @lam.1682 to i8*
  %cursor.1930 = bitcast i8* %arg1.1679 to i8*
  %sizeptr.2016 = getelementptr i64, i64* null, i32 1
  %size.2017 = ptrtoint i64* %sizeptr.2016 to i64
  %cursor.1924 = call i8* @malloc(i64 %size.2017)
  %cast.1931 = bitcast i8* %cursor.1924 to {i8*}*
  %loader.1932 = getelementptr {i8*}, {i8*}* %cast.1931, i32 0, i32 0
  store i8* %cursor.1930, i8** %loader.1932
  %sizeptr.2018 = getelementptr i64, i64* null, i32 2
  %size.2019 = ptrtoint i64* %sizeptr.2018 to i64
  %ans.1922 = call i8* @malloc(i64 %size.2019)
  %cast.1925 = bitcast i8* %ans.1922 to {i8*, i8*}*
  %loader.1928 = getelementptr {i8*, i8*}, {i8*, i8*}* %cast.1925, i32 0, i32 0
  store i8* %cursor.1923, i8** %loader.1928
  %loader.1926 = getelementptr {i8*, i8*}, {i8*, i8*}* %cast.1925, i32 0, i32 1
  store i8* %cursor.1924, i8** %loader.1926
  ret i8* %ans.1922
}
define i8* @lam.1692(i8* %env.1691, i8* %arg2.1690) {
  %base.1912 = bitcast i8* %env.1691 to i8*
  %castedBase.1913 = bitcast i8* %base.1912 to {i8*}*
  %loader.1919 = getelementptr {i8*}, {i8*}* %castedBase.1913, i32 0, i32 0
  %arg1.1689 = load i8*, i8** %loader.1919
  %arg.1914 = bitcast i8* %arg1.1689 to i8*
  %arg.1915 = bitcast i8* %arg2.1690 to i8*
  %cast.1916 = ptrtoint i8* %arg.1914 to i32
  %cast.1917 = ptrtoint i8* %arg.1915 to i32
  %result.1918 = sub i32 %cast.1916, %cast.1917
  %result.2020 = inttoptr i32 %result.1918 to i8*
  ret i8* %result.2020
}
define i8* @lam.1694(i8* %env.1693, i8* %arg1.1689) {
  %base.1898 = bitcast i8* %env.1693 to i8*
  %castedBase.1899 = bitcast i8* %base.1898 to {}*
  %cursor.1901 = bitcast i8* (i8*, i8*)* @lam.1692 to i8*
  %cursor.1908 = bitcast i8* %arg1.1689 to i8*
  %sizeptr.2021 = getelementptr i64, i64* null, i32 1
  %size.2022 = ptrtoint i64* %sizeptr.2021 to i64
  %cursor.1902 = call i8* @malloc(i64 %size.2022)
  %cast.1909 = bitcast i8* %cursor.1902 to {i8*}*
  %loader.1910 = getelementptr {i8*}, {i8*}* %cast.1909, i32 0, i32 0
  store i8* %cursor.1908, i8** %loader.1910
  %sizeptr.2023 = getelementptr i64, i64* null, i32 2
  %size.2024 = ptrtoint i64* %sizeptr.2023 to i64
  %ans.1900 = call i8* @malloc(i64 %size.2024)
  %cast.1903 = bitcast i8* %ans.1900 to {i8*, i8*}*
  %loader.1906 = getelementptr {i8*, i8*}, {i8*, i8*}* %cast.1903, i32 0, i32 0
  store i8* %cursor.1901, i8** %loader.1906
  %loader.1904 = getelementptr {i8*, i8*}, {i8*, i8*}* %cast.1903, i32 0, i32 1
  store i8* %cursor.1902, i8** %loader.1904
  ret i8* %ans.1900
}
define i8* @lam.1712(i8* %env.1711, i8* %x.1150) {
  %base.1829 = bitcast i8* %env.1711 to i8*
  %castedBase.1830 = bitcast i8* %base.1829 to {}*
  %ans.1831 = bitcast i8* %x.1150 to i8*
  %tmp.1678 = bitcast i8* %ans.1831 to i8*
  %switch.1896 = bitcast i8* %tmp.1678 to i8*
  %cast.1897 = ptrtoint i8* %switch.1896 to i64
  switch i64 %cast.1897, label %default.2025 [i64 1, label %case.2026]
case.2026:
  %ans.1832 = inttoptr i32 1 to i8*
  ret i8* %ans.1832
default.2025:
  %ans.1833 = inttoptr i32 1 to i8*
  %arg.1699 = bitcast i8* %ans.1833 to i8*
  %ans.1834 = bitcast i8* %x.1150 to i8*
  %arg.1695 = bitcast i8* %ans.1834 to i8*
  %cursor.1836 = bitcast i8* (i8*, i8*)* @lam.1694 to i8*
  %sizeptr.2027 = getelementptr i64, i64* null, i32 0
  %size.2028 = ptrtoint i64* %sizeptr.2027 to i64
  %cursor.1837 = call i8* @malloc(i64 %size.2028)
  %cast.1843 = bitcast i8* %cursor.1837 to {}*
  %sizeptr.2029 = getelementptr i64, i64* null, i32 2
  %size.2030 = ptrtoint i64* %sizeptr.2029 to i64
  %ans.1835 = call i8* @malloc(i64 %size.2030)
  %cast.1838 = bitcast i8* %ans.1835 to {i8*, i8*}*
  %loader.1841 = getelementptr {i8*, i8*}, {i8*, i8*}* %cast.1838, i32 0, i32 0
  store i8* %cursor.1836, i8** %loader.1841
  %loader.1839 = getelementptr {i8*, i8*}, {i8*, i8*}* %cast.1838, i32 0, i32 1
  store i8* %cursor.1837, i8** %loader.1839
  %fun.1696 = bitcast i8* %ans.1835 to i8*
  %base.1844 = bitcast i8* %fun.1696 to i8*
  %castedBase.1845 = bitcast i8* %base.1844 to {i8*, i8*}*
  %loader.1851 = getelementptr {i8*, i8*}, {i8*, i8*}* %castedBase.1845, i32 0, i32 0
  %down.elim.cls.1697 = load i8*, i8** %loader.1851
  %loader.1850 = getelementptr {i8*, i8*}, {i8*, i8*}* %castedBase.1845, i32 0, i32 1
  %down.elim.env.1698 = load i8*, i8** %loader.1850
  %fun.1846 = bitcast i8* %down.elim.cls.1697 to i8*
  %arg.1847 = bitcast i8* %down.elim.env.1698 to i8*
  %arg.1848 = bitcast i8* %arg.1695 to i8*
  %cast.1849 = bitcast i8* %fun.1846 to i8* (i8*, i8*)*
  %fun.1700 = call i8* %cast.1849(i8* %arg.1847, i8* %arg.1848)
  %base.1852 = bitcast i8* %fun.1700 to i8*
  %castedBase.1853 = bitcast i8* %base.1852 to {i8*, i8*}*
  %loader.1859 = getelementptr {i8*, i8*}, {i8*, i8*}* %castedBase.1853, i32 0, i32 0
  %down.elim.cls.1701 = load i8*, i8** %loader.1859
  %loader.1858 = getelementptr {i8*, i8*}, {i8*, i8*}* %castedBase.1853, i32 0, i32 1
  %down.elim.env.1702 = load i8*, i8** %loader.1858
  %fun.1854 = bitcast i8* %down.elim.cls.1701 to i8*
  %arg.1855 = bitcast i8* %down.elim.env.1702 to i8*
  %arg.1856 = bitcast i8* %arg.1699 to i8*
  %cast.1857 = bitcast i8* %fun.1854 to i8* (i8*, i8*)*
  %arg.1703 = call i8* %cast.1857(i8* %arg.1855, i8* %arg.1856)
  %fun.1860 = bitcast i8* ()* @fact.1149 to i8*
  %cast.1861 = bitcast i8* %fun.1860 to i8* ()*
  %fun.1704 = call i8* %cast.1861()
  %base.1862 = bitcast i8* %fun.1704 to i8*
  %castedBase.1863 = bitcast i8* %base.1862 to {i8*, i8*}*
  %loader.1869 = getelementptr {i8*, i8*}, {i8*, i8*}* %castedBase.1863, i32 0, i32 0
  %down.elim.cls.1705 = load i8*, i8** %loader.1869
  %loader.1868 = getelementptr {i8*, i8*}, {i8*, i8*}* %castedBase.1863, i32 0, i32 1
  %down.elim.env.1706 = load i8*, i8** %loader.1868
  %fun.1864 = bitcast i8* %down.elim.cls.1705 to i8*
  %arg.1865 = bitcast i8* %down.elim.env.1706 to i8*
  %arg.1866 = bitcast i8* %arg.1703 to i8*
  %cast.1867 = bitcast i8* %fun.1864 to i8* (i8*, i8*)*
  %arg.1707 = call i8* %cast.1867(i8* %arg.1865, i8* %arg.1866)
  %ans.1870 = bitcast i8* %x.1150 to i8*
  %arg.1685 = bitcast i8* %ans.1870 to i8*
  %cursor.1872 = bitcast i8* (i8*, i8*)* @lam.1684 to i8*
  %sizeptr.2031 = getelementptr i64, i64* null, i32 0
  %size.2032 = ptrtoint i64* %sizeptr.2031 to i64
  %cursor.1873 = call i8* @malloc(i64 %size.2032)
  %cast.1879 = bitcast i8* %cursor.1873 to {}*
  %sizeptr.2033 = getelementptr i64, i64* null, i32 2
  %size.2034 = ptrtoint i64* %sizeptr.2033 to i64
  %ans.1871 = call i8* @malloc(i64 %size.2034)
  %cast.1874 = bitcast i8* %ans.1871 to {i8*, i8*}*
  %loader.1877 = getelementptr {i8*, i8*}, {i8*, i8*}* %cast.1874, i32 0, i32 0
  store i8* %cursor.1872, i8** %loader.1877
  %loader.1875 = getelementptr {i8*, i8*}, {i8*, i8*}* %cast.1874, i32 0, i32 1
  store i8* %cursor.1873, i8** %loader.1875
  %fun.1686 = bitcast i8* %ans.1871 to i8*
  %base.1880 = bitcast i8* %fun.1686 to i8*
  %castedBase.1881 = bitcast i8* %base.1880 to {i8*, i8*}*
  %loader.1887 = getelementptr {i8*, i8*}, {i8*, i8*}* %castedBase.1881, i32 0, i32 0
  %down.elim.cls.1687 = load i8*, i8** %loader.1887
  %loader.1886 = getelementptr {i8*, i8*}, {i8*, i8*}* %castedBase.1881, i32 0, i32 1
  %down.elim.env.1688 = load i8*, i8** %loader.1886
  %fun.1882 = bitcast i8* %down.elim.cls.1687 to i8*
  %arg.1883 = bitcast i8* %down.elim.env.1688 to i8*
  %arg.1884 = bitcast i8* %arg.1685 to i8*
  %cast.1885 = bitcast i8* %fun.1882 to i8* (i8*, i8*)*
  %fun.1708 = call i8* %cast.1885(i8* %arg.1883, i8* %arg.1884)
  %base.1888 = bitcast i8* %fun.1708 to i8*
  %castedBase.1889 = bitcast i8* %base.1888 to {i8*, i8*}*
  %loader.1895 = getelementptr {i8*, i8*}, {i8*, i8*}* %castedBase.1889, i32 0, i32 0
  %down.elim.cls.1709 = load i8*, i8** %loader.1895
  %loader.1894 = getelementptr {i8*, i8*}, {i8*, i8*}* %castedBase.1889, i32 0, i32 1
  %down.elim.env.1710 = load i8*, i8** %loader.1894
  %fun.1890 = bitcast i8* %down.elim.cls.1709 to i8*
  %arg.1891 = bitcast i8* %down.elim.env.1710 to i8*
  %arg.1892 = bitcast i8* %arg.1707 to i8*
  %cast.1893 = bitcast i8* %fun.1890 to i8* (i8*, i8*)*
  %tmp.2035 = tail call i8* %cast.1893(i8* %arg.1891, i8* %arg.1892)
  ret i8* %tmp.2035
}
define i8* @fact.1149() {
  %cursor.1821 = bitcast i8* (i8*, i8*)* @lam.1712 to i8*
  %sizeptr.2036 = getelementptr i64, i64* null, i32 0
  %size.2037 = ptrtoint i64* %sizeptr.2036 to i64
  %cursor.1822 = call i8* @malloc(i64 %size.2037)
  %cast.1828 = bitcast i8* %cursor.1822 to {}*
  %sizeptr.2038 = getelementptr i64, i64* null, i32 2
  %size.2039 = ptrtoint i64* %sizeptr.2038 to i64
  %ans.1820 = call i8* @malloc(i64 %size.2039)
  %cast.1823 = bitcast i8* %ans.1820 to {i8*, i8*}*
  %loader.1826 = getelementptr {i8*, i8*}, {i8*, i8*}* %cast.1823, i32 0, i32 0
  store i8* %cursor.1821, i8** %loader.1826
  %loader.1824 = getelementptr {i8*, i8*}, {i8*, i8*}* %cast.1823, i32 0, i32 1
  store i8* %cursor.1822, i8** %loader.1824
  ret i8* %ans.1820
}
define i8* @lam.1715(i8* %env.1714, i8* %arg.1713) {
  %base.1816 = bitcast i8* %env.1714 to i8*
  %castedBase.1817 = bitcast i8* %base.1816 to {}*
  %arg.1818 = bitcast i8* %arg.1713 to i8*
  %cast.1819 = ptrtoint i8* %arg.1818 to i32
  %fmt.2041 = getelementptr [3 x i8], [3 x i8]* @fmt.i32, i32 0, i32 0
  %tmp.2042 = call i32 (i8*, ...) @printf(i8* %fmt.2041, i32 %cast.1819)
  %result.2040 = inttoptr i32 %tmp.2042 to i8*
  ret i8* %result.2040
}
define i8* @lam.1725(i8* %env.1724, i8* %fact.1151) {
  %base.1787 = bitcast i8* %env.1724 to i8*
  %castedBase.1788 = bitcast i8* %base.1787 to {}*
  %ans.1789 = inttoptr i32 10 to i8*
  %arg.1716 = bitcast i8* %ans.1789 to i8*
  %ans.1790 = bitcast i8* %fact.1151 to i8*
  %fun.1717 = bitcast i8* %ans.1790 to i8*
  %base.1791 = bitcast i8* %fun.1717 to i8*
  %castedBase.1792 = bitcast i8* %base.1791 to {i8*, i8*}*
  %loader.1798 = getelementptr {i8*, i8*}, {i8*, i8*}* %castedBase.1792, i32 0, i32 0
  %down.elim.cls.1718 = load i8*, i8** %loader.1798
  %loader.1797 = getelementptr {i8*, i8*}, {i8*, i8*}* %castedBase.1792, i32 0, i32 1
  %down.elim.env.1719 = load i8*, i8** %loader.1797
  %fun.1793 = bitcast i8* %down.elim.cls.1718 to i8*
  %arg.1794 = bitcast i8* %down.elim.env.1719 to i8*
  %arg.1795 = bitcast i8* %arg.1716 to i8*
  %cast.1796 = bitcast i8* %fun.1793 to i8* (i8*, i8*)*
  %arg.1720 = call i8* %cast.1796(i8* %arg.1794, i8* %arg.1795)
  %cursor.1800 = bitcast i8* (i8*, i8*)* @lam.1715 to i8*
  %sizeptr.2043 = getelementptr i64, i64* null, i32 0
  %size.2044 = ptrtoint i64* %sizeptr.2043 to i64
  %cursor.1801 = call i8* @malloc(i64 %size.2044)
  %cast.1807 = bitcast i8* %cursor.1801 to {}*
  %sizeptr.2045 = getelementptr i64, i64* null, i32 2
  %size.2046 = ptrtoint i64* %sizeptr.2045 to i64
  %ans.1799 = call i8* @malloc(i64 %size.2046)
  %cast.1802 = bitcast i8* %ans.1799 to {i8*, i8*}*
  %loader.1805 = getelementptr {i8*, i8*}, {i8*, i8*}* %cast.1802, i32 0, i32 0
  store i8* %cursor.1800, i8** %loader.1805
  %loader.1803 = getelementptr {i8*, i8*}, {i8*, i8*}* %cast.1802, i32 0, i32 1
  store i8* %cursor.1801, i8** %loader.1803
  %fun.1721 = bitcast i8* %ans.1799 to i8*
  %base.1808 = bitcast i8* %fun.1721 to i8*
  %castedBase.1809 = bitcast i8* %base.1808 to {i8*, i8*}*
  %loader.1815 = getelementptr {i8*, i8*}, {i8*, i8*}* %castedBase.1809, i32 0, i32 0
  %down.elim.cls.1722 = load i8*, i8** %loader.1815
  %loader.1814 = getelementptr {i8*, i8*}, {i8*, i8*}* %castedBase.1809, i32 0, i32 1
  %down.elim.env.1723 = load i8*, i8** %loader.1814
  %fun.1810 = bitcast i8* %down.elim.cls.1722 to i8*
  %arg.1811 = bitcast i8* %down.elim.env.1723 to i8*
  %arg.1812 = bitcast i8* %arg.1720 to i8*
  %cast.1813 = bitcast i8* %fun.1810 to i8* (i8*, i8*)*
  %tmp.2047 = tail call i8* %cast.1813(i8* %arg.1811, i8* %arg.1812)
  ret i8* %tmp.2047
}
define i8* @lam.1731(i8* %env.1730, i8* %io.1137) {
  %base.1766 = bitcast i8* %env.1730 to i8*
  %castedBase.1767 = bitcast i8* %base.1766 to {}*
  %fun.1768 = bitcast i8* ()* @fact.1149 to i8*
  %cast.1769 = bitcast i8* %fun.1768 to i8* ()*
  %arg.1726 = call i8* %cast.1769()
  %cursor.1771 = bitcast i8* (i8*, i8*)* @lam.1725 to i8*
  %sizeptr.2048 = getelementptr i64, i64* null, i32 0
  %size.2049 = ptrtoint i64* %sizeptr.2048 to i64
  %cursor.1772 = call i8* @malloc(i64 %size.2049)
  %cast.1778 = bitcast i8* %cursor.1772 to {}*
  %sizeptr.2050 = getelementptr i64, i64* null, i32 2
  %size.2051 = ptrtoint i64* %sizeptr.2050 to i64
  %ans.1770 = call i8* @malloc(i64 %size.2051)
  %cast.1773 = bitcast i8* %ans.1770 to {i8*, i8*}*
  %loader.1776 = getelementptr {i8*, i8*}, {i8*, i8*}* %cast.1773, i32 0, i32 0
  store i8* %cursor.1771, i8** %loader.1776
  %loader.1774 = getelementptr {i8*, i8*}, {i8*, i8*}* %cast.1773, i32 0, i32 1
  store i8* %cursor.1772, i8** %loader.1774
  %fun.1727 = bitcast i8* %ans.1770 to i8*
  %base.1779 = bitcast i8* %fun.1727 to i8*
  %castedBase.1780 = bitcast i8* %base.1779 to {i8*, i8*}*
  %loader.1786 = getelementptr {i8*, i8*}, {i8*, i8*}* %castedBase.1780, i32 0, i32 0
  %down.elim.cls.1728 = load i8*, i8** %loader.1786
  %loader.1785 = getelementptr {i8*, i8*}, {i8*, i8*}* %castedBase.1780, i32 0, i32 1
  %down.elim.env.1729 = load i8*, i8** %loader.1785
  %fun.1781 = bitcast i8* %down.elim.cls.1728 to i8*
  %arg.1782 = bitcast i8* %down.elim.env.1729 to i8*
  %arg.1783 = bitcast i8* %arg.1726 to i8*
  %cast.1784 = bitcast i8* %fun.1781 to i8* (i8*, i8*)*
  %tmp.2052 = tail call i8* %cast.1784(i8* %arg.1782, i8* %arg.1783)
  ret i8* %tmp.2052
}
define i8* @lam.1738(i8* %env.1737, i8* %state.1116) {
  %base.1743 = bitcast i8* %env.1737 to i8*
  %castedBase.1744 = bitcast i8* %base.1743 to {}*
  %ans.1745 = bitcast i8* %state.1116 to i8*
  %arg.1732 = bitcast i8* %ans.1745 to i8*
  %fun.1746 = bitcast i8* (i8*)* @io.1136 to i8*
  %arg.1747 = bitcast i8* %arg.1732 to i8*
  %cast.1748 = bitcast i8* %fun.1746 to i8* (i8*)*
  %arg.1733 = call i8* %cast.1748(i8* %arg.1747)
  %cursor.1750 = bitcast i8* (i8*, i8*)* @lam.1731 to i8*
  %sizeptr.2053 = getelementptr i64, i64* null, i32 0
  %size.2054 = ptrtoint i64* %sizeptr.2053 to i64
  %cursor.1751 = call i8* @malloc(i64 %size.2054)
  %cast.1757 = bitcast i8* %cursor.1751 to {}*
  %sizeptr.2055 = getelementptr i64, i64* null, i32 2
  %size.2056 = ptrtoint i64* %sizeptr.2055 to i64
  %ans.1749 = call i8* @malloc(i64 %size.2056)
  %cast.1752 = bitcast i8* %ans.1749 to {i8*, i8*}*
  %loader.1755 = getelementptr {i8*, i8*}, {i8*, i8*}* %cast.1752, i32 0, i32 0
  store i8* %cursor.1750, i8** %loader.1755
  %loader.1753 = getelementptr {i8*, i8*}, {i8*, i8*}* %cast.1752, i32 0, i32 1
  store i8* %cursor.1751, i8** %loader.1753
  %fun.1734 = bitcast i8* %ans.1749 to i8*
  %base.1758 = bitcast i8* %fun.1734 to i8*
  %castedBase.1759 = bitcast i8* %base.1758 to {i8*, i8*}*
  %loader.1765 = getelementptr {i8*, i8*}, {i8*, i8*}* %castedBase.1759, i32 0, i32 0
  %down.elim.cls.1735 = load i8*, i8** %loader.1765
  %loader.1764 = getelementptr {i8*, i8*}, {i8*, i8*}* %castedBase.1759, i32 0, i32 1
  %down.elim.env.1736 = load i8*, i8** %loader.1764
  %fun.1760 = bitcast i8* %down.elim.cls.1735 to i8*
  %arg.1761 = bitcast i8* %down.elim.env.1736 to i8*
  %arg.1762 = bitcast i8* %arg.1733 to i8*
  %cast.1763 = bitcast i8* %fun.1760 to i8* (i8*, i8*)*
  %tmp.2057 = tail call i8* %cast.1763(i8* %arg.1761, i8* %arg.1762)
  ret i8* %tmp.2057
}