BEGIN;
    CREATE TABLE Employee (Fname, Minit, Lname, Ssn PRIMARY KEY, Bdate,
	Address, Sex, Salary, Super_ssn, Dno, FOREIGN KEY (Super_ssn)
	REFERENCES Employee (Ssn));
    CREATE TABLE Department (Dname, Dnumber PRIMARY KEY, Mgr_ssn, Mgr_start_date, FOREIGN KEY (Mgr_ssn) REFERENCES Employee (Ssn));
    CREATE TABLE Dept_locations (Dnumber, Dlocation, PRIMARY KEY (Dnumber, Dlocation), FOREIGN KEY (Dnumber) REFERENCES Department (Dnumber));
    CREATE TABLE Works_on (Essn, Pno, Hours, PRIMARY KEY (Essn, Pno), FOREIGN KEY (Essn) REFERENCES Employee (Ssn));
    CREATE TABLE Project (Pname, Pnumber PRIMARY KEY, Plocation, Dnum, FOREIGN KEY (Dnum) REFERENCES Department (Dnumber));
    CREATE TABLE Dependent (Essn, Dependent_name, Sex, Bdate, Relationship, PRIMARY KEY (Essn, Dependent_name), FOREIGN KEY (Essn) REFERENCES Emplyee (Essn));

    INSERT INTO Employee VALUES ('John'     , 'B' , 'Smith'   , '123456789' , '1965-01-09' , '731 Fondren, Houston, TX' , 'M' , 30000 , '333445555' , 5);
    INSERT INTO Employee VALUES ('Franklin' , 'T' , 'Wong'    , '333445555' , '1955-12-08' , '638 Voss, Houston, TX' , 'M' , 40000 , '888865555' , 5);
    INSERT INTO Employee VALUES ('Alicia'   , 'J' , 'Zelaya'  , '999887777' , '1968-01-19' , '3321 Castle, Spring, TX' , 'F' , 25000 , '987654321' , 4);
    INSERT INTO Employee VALUES ('Jennifer' , 'S' , 'Wallace' , '987654321' , '1941-06-20' , '291 Berry, Bellaire, TX' , 'F' , 43000 , '888865555' , 4);
    INSERT INTO Employee VALUES ('Ramesh'   , 'K' , 'Narayan' , '666884444' , '1962-09-15' , '975 Fire Oak, Humble, TX' , 'M' , 38000 , '333445555' , 5);
    INSERT INTO Employee VALUES ('Joyce'    , 'A' , 'English' , '453459453' , '1972-07-31' , '5631 Rice, Houston, TX' , 'F' , 25000 , '333445555' , 5);
    INSERT INTO Employee VALUES ('Ahmad'    , 'V' , 'Jabbar'  , '987987987' , '1969-03-29' , '980 Dallas, Houston, TX' , 'M' , 25000 , '987654321' , 4);
    INSERT INTO Employee VALUES ('James'    , 'E' , 'Borg'    , '888865555' , '1937-11-10' , '450 Stone, Houston, TX' , 'M' , 55000 , NULL        , 1);

    INSERT INTO Department VALUES ('Research'       , 5 , 333445555 , '1988-05-22');
    INSERT INTO Department VALUES ('Administration' , 4 , 987654321 , '1995-01-01');
    INSERT INTO Department VALUES ('Headquarters'   , 1 , 888865555 , '1981-06-19');

    INSERT INTO Dept_locations VALUES (1, 'Houston');
    INSERT INTO Dept_locations VALUES (4, 'Stafford');
    INSERT INTO Dept_locations VALUES (5, 'Bellaire');
    INSERT INTO Dept_locations VALUES (5, 'Sugarland');
    INSERT INTO Dept_locations VALUES (5, 'Houston');

    INSERT INTO Works_on VALUES ('123456789' , 1  , 32.5);
    INSERT INTO Works_on VALUES ('123456789' , 2  ,  7.5);
    INSERT INTO Works_on VALUES ('666884444' , 3  , 40.0);
    INSERT INTO Works_on VALUES ('453459453' , 1  , 20.0);
    INSERT INTO Works_on VALUES ('453459453' , 2  , 20.0);
    INSERT INTO Works_on VALUES ('333445555' , 2  , 10.0);
    INSERT INTO Works_on VALUES ('333445555' , 3  , 10.0);
    INSERT INTO Works_on VALUES ('333445555' , 10 , 10.0);
    INSERT INTO Works_on VALUES ('333445555' , 20 , 10.0);
    INSERT INTO Works_on VALUES ('999887777' , 30 , 30.0);
    INSERT INTO Works_on VALUES ('999887777' , 10 , 10.0);
    INSERT INTO Works_on VALUES ('987987987' , 10 , 35.0);
    INSERT INTO Works_on VALUES ('987987987' , 30 ,  5.0);
    INSERT INTO Works_on VALUES ('987654321' , 30 , 20.0);
    INSERT INTO Works_on VALUES ('987654321' , 20 , 15.0);
    INSERT INTO Works_on VALUES ('888865555' , 20 , NULL);

    INSERT INTO PROJECT VALUES ('ProductX'        , 1  , 'Bellaire'  , 5);
    INSERT INTO PROJECT VALUES ('ProductY'        , 2  , 'Sugarland' , 5);
    INSERT INTO PROJECT VALUES ('ProductZ'        , 3  , 'Houston'   , 5);
    INSERT INTO PROJECT VALUES ('Computerization' , 10 , 'Stafford'  , 4);
    INSERT INTO PROJECT VALUES ('Reorganization'  , 20 , 'Houston'   , 1);
    INSERT INTO PROJECT VALUES ('Newbenefits'     , 30 , 'Stafford'  , 4);

    INSERT INTO Dependent VALUES ('333445555' , 'Alice'     , 'F' , '1986-04-05' , 'Daughter');
    INSERT INTO Dependent VALUES ('333445555' , 'Theodore'  , 'M' , '1983-10-25' , 'Son');
    INSERT INTO Dependent VALUES ('333445555' , 'Joy'       , 'F' , '1958-05-03' , 'Spouse');
    INSERT INTO Dependent VALUES ('987654321' , 'Abner'     , 'M' , '1942-02-28' , 'Spouse');
    INSERT INTO Dependent VALUES ('123456789' , 'Michael'   , 'M' , '1988-01-04' , 'Son');
    INSERT INTO Dependent VALUES ('123456789' , 'Alice'     , 'F' , '1988-12-30' , 'Daughter');
    INSERT INTO Dependent VALUES ('123456789' , 'Elizabeth' , 'F' , '1967-05-05' , 'Spouse');
--ROLLBACK; -- Delete or comment when done
COMMIT;
