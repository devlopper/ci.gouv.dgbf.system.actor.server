package ci.gouv.dgbf.system.actor.server.persistence.impl.unit;

import static org.assertj.core.api.Assertions.assertThat;

import java.time.LocalDateTime;
import java.util.List;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.persistence.query.EntityCounter;
import org.cyk.utility.persistence.query.EntityReader;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.RequestQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Request;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestScopeFunction;

public class RequestPersistenceImplUnitTest extends AbstractUnitTestMemory {
	private static final long serialVersionUID = 1L;

	@Override
	protected String getPersistenceUnitName() {
		return "requests";
	}

	@Test
	public void read_dynamic() {
		assertThat(EntityCounter.getInstance().count(Request.class, new QueryExecutorArguments().queryCountDynamic(Request.class))).isEqualTo(3l);
		assertThat(EntityCounter.getInstance().count(RequestScopeFunction.class, new QueryExecutorArguments().queryCountDynamic(RequestScopeFunction.class))).isEqualTo(6l);
		QueryExecutorArguments arguments = new QueryExecutorArguments().queryReadDynamic(Request.class);
		arguments.addProjectionsFromStrings(Request.FIELD_IDENTIFIER,Request.FIELD_CODE,Request.FIELD_FIRST_NAME,Request.FIELD_LAST_NAMES,Request.FIELD_REGISTRATION_NUMBER
				,Request.FIELD_ELECTRONIC_MAIL_ADDRESS)
		.addProcessableTransientFieldsNames(Request.FIELDS_SECTION_AS_CODE_ADMINISTRATIVE_UNIT_AS_CODE_TYPE_STATUS_CREATION_DATE_PROCESSING_DATE_AS_STRINGS
				,Request.FIELDS_SCOPE_FUNCTIONS_CODES,Request.FIELDS_GRANTED_SCOPE_FUNCTIONS_CODES);
		assertRequests((List<Request>) EntityReader.getInstance().readMany(Request.class, arguments), new Object[][] {
			{"1","Komenan","Yao Christian","498721Y","kycdev@gmail.com","327","13010222","01/01/2000 à 00:00","02/01/2000 à 00:00","Accepté",new String[]{"GCDTI","AGCDTI"},new String[]{"GCDTI"}}
			,{"2","Komenan","Yao Christian","498721Y","kycdev@gmail.com","327","13010222","02/01/2000 à 00:00",null,"Initié",new String[]{"AGCDTI"},null}
			,{"3","Zadi","Gérard","100100A","test@mail.com","323","13010220","01/01/2000 à 00:00",null,"Initié",new String[]{"GCDTI","ORDBUDGET"},null}
		});
	}
	
	@Test
	public void read_dynamic_administrative_unit_section_327() {
		QueryExecutorArguments arguments = new QueryExecutorArguments().queryReadDynamic(Request.class);
		arguments.addProjectionsFromStrings(Request.FIELD_IDENTIFIER,Request.FIELD_CODE,Request.FIELD_FIRST_NAME,Request.FIELD_LAST_NAMES,Request.FIELD_REGISTRATION_NUMBER
				,Request.FIELD_ELECTRONIC_MAIL_ADDRESS)
		.addProcessableTransientFieldsNames(Request.FIELDS_SECTION_ADMINISTRATIVE_UNIT_TYPE_STATUS_CREATION_DATE_PROCESSING_DATE_AS_STRINGS
				,Request.FIELDS_SCOPE_FUNCTIONS_CODES,Request.FIELDS_GRANTED_SCOPE_FUNCTIONS_CODES);
		arguments.addFilterField(RequestQuerier.PARAMETER_NAME_ADMINISTRATIVE_UNITS_SECTIONS_IDENTIFIERS, List.of("327"));
		assertRequests((List<Request>) EntityReader.getInstance().readMany(Request.class, arguments), new Object[][] {
			{"1","Komenan","Yao Christian","498721Y","kycdev@gmail.com","327 Budget","13010222 DTI","01/01/2000 à 00:00","02/01/2000 à 00:00","Accepté",new String[]{"GCDTI","AGCDTI"},new String[]{"GCDTI"}}
			,{"2","Komenan","Yao Christian","498721Y","kycdev@gmail.com","327 Budget","13010222 DTI","02/01/2000 à 00:00",null,"Initié",new String[]{"AGCDTI"},null}
		});
	}
	
	@Test
	public void read_dynamic_administrative_unit_section_323() {
		QueryExecutorArguments arguments = new QueryExecutorArguments().queryReadDynamic(Request.class);
		arguments.addProjectionsFromStrings(Request.FIELD_IDENTIFIER,Request.FIELD_CODE,Request.FIELD_FIRST_NAME,Request.FIELD_LAST_NAMES,Request.FIELD_REGISTRATION_NUMBER
				,Request.FIELD_ELECTRONIC_MAIL_ADDRESS)
		.addProcessableTransientFieldsNames(Request.FIELDS_SECTION_ADMINISTRATIVE_UNIT_TYPE_STATUS_CREATION_DATE_PROCESSING_DATE_AS_STRINGS
				,Request.FIELDS_SCOPE_FUNCTIONS_CODES,Request.FIELDS_GRANTED_SCOPE_FUNCTIONS_CODES);
		arguments.addFilterField(RequestQuerier.PARAMETER_NAME_ADMINISTRATIVE_UNITS_SECTIONS_IDENTIFIERS, List.of("323"));
		assertRequests((List<Request>) EntityReader.getInstance().readMany(Request.class, arguments), new Object[][] {
			{"3","Zadi","Gérard","100100A","test@mail.com","323 Intérieur","13010220 DGDDL","01/01/2000 à 00:00",null,"Initié",new String[]{"GCDTI","ORDBUDGET"},null}
		});
	}
	
	@Test
	public void read_dynamic_function_gc() {
		QueryExecutorArguments arguments = new QueryExecutorArguments().queryReadDynamic(Request.class);
		arguments.addProjectionsFromStrings(Request.FIELD_IDENTIFIER,Request.FIELD_CODE,Request.FIELD_FIRST_NAME,Request.FIELD_LAST_NAMES,Request.FIELD_REGISTRATION_NUMBER
				,Request.FIELD_ELECTRONIC_MAIL_ADDRESS)
		.addProcessableTransientFieldsNames(Request.FIELDS_SECTION_ADMINISTRATIVE_UNIT_TYPE_STATUS_CREATION_DATE_PROCESSING_DATE_AS_STRINGS
				,Request.FIELDS_SCOPE_FUNCTIONS_CODES,Request.FIELDS_GRANTED_SCOPE_FUNCTIONS_CODES);
		arguments.addFilterField(RequestQuerier.PARAMETER_NAME_FUNCTIONS_IDENTIFIERS, List.of("GC"));
		assertRequests((List<Request>) EntityReader.getInstance().readMany(Request.class, arguments), new Object[][] {
			{"1","Komenan","Yao Christian","498721Y","kycdev@gmail.com","327 Budget","13010222 DTI","01/01/2000 à 00:00","02/01/2000 à 00:00","Accepté",new String[]{"GCDTI","AGCDTI"},new String[]{"GCDTI"}}
			,{"3","Zadi","Gérard","100100A","test@mail.com","323 Intérieur","13010220 DGDDL","01/01/2000 à 00:00",null,"Initié",new String[]{"GCDTI","ORDBUDGET"},null}
		});
	}
	
	@Test
	public void read_dynamic_function_gc_accepted() {
		QueryExecutorArguments arguments = new QueryExecutorArguments().queryReadDynamic(Request.class);
		arguments.addProjectionsFromStrings(Request.FIELD_IDENTIFIER,Request.FIELD_CODE,Request.FIELD_FIRST_NAME,Request.FIELD_LAST_NAMES,Request.FIELD_REGISTRATION_NUMBER
				,Request.FIELD_ELECTRONIC_MAIL_ADDRESS)
		.addProcessableTransientFieldsNames(Request.FIELDS_SECTION_ADMINISTRATIVE_UNIT_TYPE_STATUS_CREATION_DATE_PROCESSING_DATE_AS_STRINGS
				,Request.FIELDS_SCOPE_FUNCTIONS_CODES,Request.FIELDS_GRANTED_SCOPE_FUNCTIONS_CODES);
		arguments.addFilterField(RequestQuerier.PARAMETER_NAME_FUNCTIONS_IDENTIFIERS, List.of("GC"));
		arguments.addFilterField(RequestQuerier.PARAMETER_NAME_STATUS_IDENTIFIERS, List.of("A"));
		assertRequests((List<Request>) EntityReader.getInstance().readMany(Request.class, arguments), new Object[][] {
			{"1","Komenan","Yao Christian","498721Y","kycdev@gmail.com","327 Budget","13010222 DTI","01/01/2000 à 00:00","02/01/2000 à 00:00","Accepté",new String[]{"GCDTI","AGCDTI"},new String[]{"GCDTI"}}
		});
	}
	
	@Test
	public void read_dynamic_function_agc() {
		QueryExecutorArguments arguments = new QueryExecutorArguments().queryReadDynamic(Request.class);
		arguments.addProjectionsFromStrings(Request.FIELD_IDENTIFIER,Request.FIELD_CODE,Request.FIELD_FIRST_NAME,Request.FIELD_LAST_NAMES,Request.FIELD_REGISTRATION_NUMBER
				,Request.FIELD_ELECTRONIC_MAIL_ADDRESS)
		.addProcessableTransientFieldsNames(Request.FIELDS_SECTION_ADMINISTRATIVE_UNIT_TYPE_STATUS_CREATION_DATE_PROCESSING_DATE_AS_STRINGS
				,Request.FIELDS_SCOPE_FUNCTIONS_CODES,Request.FIELDS_GRANTED_SCOPE_FUNCTIONS_CODES);
		arguments.addFilterField(RequestQuerier.PARAMETER_NAME_FUNCTIONS_IDENTIFIERS, List.of("AGC"));
		assertRequests((List<Request>) EntityReader.getInstance().readMany(Request.class, arguments), new Object[][] {
			{"1","Komenan","Yao Christian","498721Y","kycdev@gmail.com","327 Budget","13010222 DTI","01/01/2000 à 00:00","02/01/2000 à 00:00","Accepté",new String[]{"GCDTI","AGCDTI"},new String[]{"GCDTI"}}
			,{"2","Komenan","Yao Christian","498721Y","kycdev@gmail.com","327 Budget","13010222 DTI","02/01/2000 à 00:00",null,"Initié",new String[]{"AGCDTI"},null}
		});
	}
	
	@Test
	public void read_dynamic_function_ord() {
		QueryExecutorArguments arguments = new QueryExecutorArguments().queryReadDynamic(Request.class);
		arguments.addProjectionsFromStrings(Request.FIELD_IDENTIFIER,Request.FIELD_CODE,Request.FIELD_FIRST_NAME,Request.FIELD_LAST_NAMES,Request.FIELD_REGISTRATION_NUMBER
				,Request.FIELD_ELECTRONIC_MAIL_ADDRESS)
		.addProcessableTransientFieldsNames(Request.FIELDS_SECTION_ADMINISTRATIVE_UNIT_TYPE_STATUS_CREATION_DATE_PROCESSING_DATE_AS_STRINGS
				,Request.FIELDS_SCOPE_FUNCTIONS_CODES,Request.FIELDS_GRANTED_SCOPE_FUNCTIONS_CODES);
		arguments.addFilterField(RequestQuerier.PARAMETER_NAME_FUNCTIONS_IDENTIFIERS, List.of("ORD"));
		assertRequests((List<Request>) EntityReader.getInstance().readMany(Request.class, arguments), new Object[][] {
			{"3","Zadi","Gérard","100100A","test@mail.com","323 Intérieur","13010220 DGDDL","01/01/2000 à 00:00",null,"Initié",new String[]{"GCDTI","ORDBUDGET"},null}
		});
	}
	
	@Test
	public void read_dynamic_type_DPA() {
		QueryExecutorArguments arguments = new QueryExecutorArguments().queryReadDynamic(Request.class);
		arguments.addProjectionsFromStrings(Request.FIELD_IDENTIFIER,Request.FIELD_CODE,Request.FIELD_FIRST_NAME,Request.FIELD_LAST_NAMES,Request.FIELD_REGISTRATION_NUMBER
				,Request.FIELD_ELECTRONIC_MAIL_ADDRESS)
		.addProcessableTransientFieldsNames(Request.FIELDS_SECTION_ADMINISTRATIVE_UNIT_TYPE_STATUS_CREATION_DATE_PROCESSING_DATE_AS_STRINGS
				,Request.FIELDS_SCOPE_FUNCTIONS_CODES,Request.FIELDS_GRANTED_SCOPE_FUNCTIONS_CODES);
		arguments.addFilterField(RequestQuerier.PARAMETER_NAME_TYPES_IDENTIFIERS, List.of("DPA"));
		assertRequests((List<Request>) EntityReader.getInstance().readMany(Request.class, arguments), new Object[][] {
			{"2","Komenan","Yao Christian","498721Y","kycdev@gmail.com","327 Budget","13010222 DTI","02/01/2000 à 00:00",null,"Initié",new String[]{"AGCDTI"},null}
		});
	}
	
	@Test
	public void read_dynamic_type_DPB() {
		QueryExecutorArguments arguments = new QueryExecutorArguments().queryReadDynamic(Request.class);
		arguments.addProjectionsFromStrings(Request.FIELD_IDENTIFIER,Request.FIELD_CODE,Request.FIELD_FIRST_NAME,Request.FIELD_LAST_NAMES,Request.FIELD_REGISTRATION_NUMBER
				,Request.FIELD_ELECTRONIC_MAIL_ADDRESS)
		.addProcessableTransientFieldsNames(Request.FIELDS_SECTION_ADMINISTRATIVE_UNIT_TYPE_STATUS_CREATION_DATE_PROCESSING_DATE_AS_STRINGS
				,Request.FIELDS_SCOPE_FUNCTIONS_CODES,Request.FIELDS_GRANTED_SCOPE_FUNCTIONS_CODES);
		arguments.addFilterField(RequestQuerier.PARAMETER_NAME_TYPES_IDENTIFIERS, List.of("DPB"));
		assertRequests((List<Request>) EntityReader.getInstance().readMany(Request.class, arguments), new Object[][] {
			{"1","Komenan","Yao Christian","498721Y","kycdev@gmail.com","327 Budget","13010222 DTI","01/01/2000 à 00:00","02/01/2000 à 00:00","Accepté",new String[]{"GCDTI","AGCDTI"},new String[]{"GCDTI"}}
			,{"3","Zadi","Gérard","100100A","test@mail.com","323 Intérieur","13010220 DGDDL","01/01/2000 à 00:00",null,"Initié",new String[]{"GCDTI","ORDBUDGET"},null}
		});
	}
	
	@Test
	public void read_dynamic_status_I() {
		QueryExecutorArguments arguments = new QueryExecutorArguments().queryReadDynamic(Request.class);
		arguments.addProjectionsFromStrings(Request.FIELD_IDENTIFIER,Request.FIELD_CODE,Request.FIELD_FIRST_NAME,Request.FIELD_LAST_NAMES,Request.FIELD_REGISTRATION_NUMBER
				,Request.FIELD_ELECTRONIC_MAIL_ADDRESS)
		.addProcessableTransientFieldsNames(Request.FIELDS_SECTION_ADMINISTRATIVE_UNIT_TYPE_STATUS_CREATION_DATE_PROCESSING_DATE_AS_STRINGS
				,Request.FIELDS_SCOPE_FUNCTIONS_CODES,Request.FIELDS_GRANTED_SCOPE_FUNCTIONS_CODES);
		arguments.addFilterField(RequestQuerier.PARAMETER_NAME_STATUS_IDENTIFIERS, List.of("I"));
		assertRequests((List<Request>) EntityReader.getInstance().readMany(Request.class, arguments), new Object[][] {
			{"2","Komenan","Yao Christian","498721Y","kycdev@gmail.com","327 Budget","13010222 DTI","02/01/2000 à 00:00",null,"Initié",new String[]{"AGCDTI"},null}
			,{"3","Zadi","Gérard","100100A","test@mail.com","323 Intérieur","13010220 DGDDL","01/01/2000 à 00:00",null,"Initié",new String[]{"GCDTI","ORDBUDGET"},null}
		});
	}
	
	@Test
	public void read_dynamic_status_A() {
		QueryExecutorArguments arguments = new QueryExecutorArguments().queryReadDynamic(Request.class);
		arguments.addProjectionsFromStrings(Request.FIELD_IDENTIFIER,Request.FIELD_CODE,Request.FIELD_FIRST_NAME,Request.FIELD_LAST_NAMES,Request.FIELD_REGISTRATION_NUMBER
				,Request.FIELD_ELECTRONIC_MAIL_ADDRESS)
		.addProcessableTransientFieldsNames(Request.FIELDS_SECTION_ADMINISTRATIVE_UNIT_TYPE_STATUS_CREATION_DATE_PROCESSING_DATE_AS_STRINGS
				,Request.FIELDS_SCOPE_FUNCTIONS_CODES,Request.FIELDS_GRANTED_SCOPE_FUNCTIONS_CODES);
		arguments.addFilterField(RequestQuerier.PARAMETER_NAME_STATUS_IDENTIFIERS, List.of("A"));
		assertRequests((List<Request>) EntityReader.getInstance().readMany(Request.class, arguments), new Object[][] {
			{"1","Komenan","Yao Christian","498721Y","kycdev@gmail.com","327 Budget","13010222 DTI","01/01/2000 à 00:00","02/01/2000 à 00:00","Accepté",new String[]{"GCDTI","AGCDTI"},new String[]{"GCDTI"}}
		});
	}
	
	@Test
	public void read_dynamic_creation_date_lower_2000_1_2() {
		QueryExecutorArguments arguments = new QueryExecutorArguments().queryReadDynamic(Request.class);
		arguments.addProjectionsFromStrings(Request.FIELD_IDENTIFIER,Request.FIELD_CODE,Request.FIELD_FIRST_NAME,Request.FIELD_LAST_NAMES,Request.FIELD_REGISTRATION_NUMBER
				,Request.FIELD_ELECTRONIC_MAIL_ADDRESS)
		.addProcessableTransientFieldsNames(Request.FIELDS_SECTION_ADMINISTRATIVE_UNIT_TYPE_STATUS_CREATION_DATE_PROCESSING_DATE_AS_STRINGS
				,Request.FIELDS_SCOPE_FUNCTIONS_CODES,Request.FIELDS_GRANTED_SCOPE_FUNCTIONS_CODES);
		arguments.addFilterField(RequestQuerier.PARAMETER_NAME_LOWEST_CREATION_DATE, LocalDateTime.of(2000, 1, 2, 0, 0));
		assertRequests((List<Request>) EntityReader.getInstance().readMany(Request.class, arguments), new Object[][] {
			{"2","Komenan","Yao Christian","498721Y","kycdev@gmail.com","327 Budget","13010222 DTI","02/01/2000 à 00:00",null,"Initié",new String[]{"AGCDTI"},null}
		});
	}
	
	/**/
	
	public static void assertRequests(List<Request> requests,Object[][] expectedStrings) {
		if(CollectionHelper.isEmpty(requests))
			assertThat(expectedStrings).isNull();
		else {
			assertThat(expectedStrings).as("nombre de demandes").hasSize(requests.size());
			for(Integer index = 0; index < requests.size(); index = index + 1) {
				Request request = requests.get(index);
				assertThat(request.getCode()).as("Code").isEqualTo(expectedStrings[index][0]);
				assertThat(request.getFirstName()).as("Nom").isEqualTo(expectedStrings[index][1]);
				assertThat(request.getLastNames()).as("Prénoms").isEqualTo(expectedStrings[index][2]);
				assertThat(request.getRegistrationNumber()).as("Matricule").isEqualTo(expectedStrings[index][3]);
				assertThat(request.getElectronicMailAddress()).as("Email").isEqualTo(expectedStrings[index][4]);
				assertThat(request.getSectionAsString()).as("Section").isEqualTo(expectedStrings[index][5]);
				assertThat(request.getAdministrativeUnitAsString()).as("Unité administrative").isEqualTo(expectedStrings[index][6]);
				assertThat(request.getCreationDateAsString()).as("Date de création").isEqualTo(expectedStrings[index][7]);
				assertThat(request.getProcessingDateAsString()).as("Date de traitement").isEqualTo(expectedStrings[index][8]);
				assertThat(request.getStatusAsString()).as("Statut").isEqualTo(expectedStrings[index][9]);
				
				if(CollectionHelper.isEmpty(request.getScopeFunctionsCodes()))
					assertThat(expectedStrings[index][10]).as("Postes demandées").isNull();
				else
					assertThat(request.getScopeFunctionsCodes()).as("Postes demandées").containsOnly((String[])expectedStrings[index][10]);
				
				if(CollectionHelper.isEmpty(request.getGrantedScopeFunctionsCodes()))
					assertThat(expectedStrings[index][11]).as("Postes accordées").isNull();
				else
					assertThat(request.getGrantedScopeFunctionsCodes()).as("Postes accordées").containsOnly((String[])expectedStrings[index][11]);
			}
		}
	}
}