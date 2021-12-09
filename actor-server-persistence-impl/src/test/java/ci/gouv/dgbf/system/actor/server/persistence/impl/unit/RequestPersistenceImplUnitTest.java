package ci.gouv.dgbf.system.actor.server.persistence.impl.unit;

import static org.assertj.core.api.Assertions.assertThat;

import java.time.LocalDateTime;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.computation.SortOrder;
import org.cyk.utility.persistence.query.EntityCounter;
import org.cyk.utility.persistence.query.EntityReader;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.RequestDispatchSlipQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.RequestQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.RequestStatusQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Request;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestDispatchSlip;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestStatus;

public class RequestPersistenceImplUnitTest extends AbstractUnitTestMemory {
	private static final long serialVersionUID = 1L;

	@Override
	protected String getPersistenceUnitName() {
		return "requests";
	}
	
	@Test
	public void requestDispatchSlip_read_dynamic() {
		assertThat(EntityCounter.getInstance().count(RequestDispatchSlip.class, new QueryExecutorArguments().queryCountDynamic(RequestDispatchSlip.class))).isEqualTo(3l);
		QueryExecutorArguments arguments = new QueryExecutorArguments().queryReadDynamic(RequestDispatchSlip.class);
		arguments.addProjectionsFromStrings(RequestDispatchSlip.FIELD_IDENTIFIER,RequestDispatchSlip.FIELD_CODE,RequestDispatchSlip.FIELD_NAME
				,RequestDispatchSlip.FIELD_NUMBER_OF_REQUESTS).addProcessableTransientFieldsNames(RequestDispatchSlip.FIELDS_SECTION_AS_CODE_FUNCTION_AS_CODE_ALL_DATES_ALL_NUMBERS_OF_REQUESTS_AS_STRING);
		assertRequestDispatchSlipAsStrings((List<RequestDispatchSlip>) EntityReader.getInstance().readMany(RequestDispatchSlip.class, arguments), new Object[][] {
			{"B001","B001","327","GC",1}
			,{"B002","B002","327","GC",0}
			,{"B003","B003","327","GC",0}
		});
	}
	
	@Test
	public void requestDispatchSlip_read_dynamic_sent() {
		QueryExecutorArguments arguments = new QueryExecutorArguments().queryReadDynamic(RequestDispatchSlip.class);
		arguments.addProjectionsFromStrings(RequestDispatchSlip.FIELD_IDENTIFIER,RequestDispatchSlip.FIELD_CODE,RequestDispatchSlip.FIELD_NAME
				,RequestDispatchSlip.FIELD_NUMBER_OF_REQUESTS).addProcessableTransientFieldsNames(RequestDispatchSlip.FIELDS_SECTION_FUNCTION)
			.addFilterFieldsValues(RequestDispatchSlipQuerier.PARAMETER_NAME_SENT,Boolean.TRUE);
		assertRequestDispatchSlip((List<RequestDispatchSlip>) EntityReader.getInstance().readMany(RequestDispatchSlip.class, arguments), new Object[][] {
			{"B002","B002","327","GC",0}
			,{"B003","B003","327","GC",0}
		});
	}
	
	@Test
	public void requestDispatchSlip_read_dynamic_notSent() {
		QueryExecutorArguments arguments = new QueryExecutorArguments().queryReadDynamic(RequestDispatchSlip.class);
		arguments.addProjectionsFromStrings(RequestDispatchSlip.FIELD_IDENTIFIER,RequestDispatchSlip.FIELD_CODE,RequestDispatchSlip.FIELD_NAME
				,RequestDispatchSlip.FIELD_NUMBER_OF_REQUESTS).addProcessableTransientFieldsNames(RequestDispatchSlip.FIELDS_SECTION_FUNCTION)
			.addFilterFieldsValues(RequestDispatchSlipQuerier.PARAMETER_NAME_SENT,Boolean.FALSE);
		assertRequestDispatchSlip((List<RequestDispatchSlip>) EntityReader.getInstance().readMany(RequestDispatchSlip.class, arguments), new Object[][] {
			{"B001","B001","327","GC",1}
		});
	}
	
	@Test
	public void requestDispatchSlip_read_dynamic_processed() {
		QueryExecutorArguments arguments = new QueryExecutorArguments().queryReadDynamic(RequestDispatchSlip.class);
		arguments.addProjectionsFromStrings(RequestDispatchSlip.FIELD_IDENTIFIER,RequestDispatchSlip.FIELD_CODE,RequestDispatchSlip.FIELD_NAME
				,RequestDispatchSlip.FIELD_NUMBER_OF_REQUESTS).addProcessableTransientFieldsNames(RequestDispatchSlip.FIELDS_SECTION_FUNCTION)
			.addFilterFieldsValues(RequestDispatchSlipQuerier.PARAMETER_NAME_PROCESSED,Boolean.TRUE);
		assertRequestDispatchSlip((List<RequestDispatchSlip>) EntityReader.getInstance().readMany(RequestDispatchSlip.class, arguments), new Object[][] {
			{"B003","B003","327","GC",0}
		});
	}
	
	@Test
	public void requestDispatchSlip_read_dynamic_notProcessed() {
		QueryExecutorArguments arguments = new QueryExecutorArguments().queryReadDynamic(RequestDispatchSlip.class);
		arguments.addProjectionsFromStrings(RequestDispatchSlip.FIELD_IDENTIFIER,RequestDispatchSlip.FIELD_CODE,RequestDispatchSlip.FIELD_NAME
				,RequestDispatchSlip.FIELD_NUMBER_OF_REQUESTS).addProcessableTransientFieldsNames(RequestDispatchSlip.FIELDS_SECTION_FUNCTION)
			.addFilterFieldsValues(RequestDispatchSlipQuerier.PARAMETER_NAME_PROCESSED,Boolean.FALSE);
		assertRequestDispatchSlip((List<RequestDispatchSlip>) EntityReader.getInstance().readMany(RequestDispatchSlip.class, arguments), new Object[][] {
			{"B001","B001","327","GC",1}
			,{"B002","B002","327","GC",0}
		});
	}
	
	@Test
	public void requestStatus_read_dynamic() {
		assertThat(EntityCounter.getInstance().count(RequestStatus.class, new QueryExecutorArguments().queryCountDynamic(RequestStatus.class))).isEqualTo(4l);
		QueryExecutorArguments arguments = new QueryExecutorArguments().queryReadDynamic(RequestStatus.class);
		arguments.addProjectionsFromStrings(RequestStatus.FIELD_IDENTIFIER,RequestStatus.FIELD_CODE,RequestStatus.FIELD_NAME);
		assertRequestStatus((List<RequestStatus>) EntityReader.getInstance().readMany(RequestStatus.class, arguments), new Object[][] {
			{"ACCEPTEE","Accepté"},{"INITIEE","Initié"},{"REJETEE","Rejeté"},{"SOUMISE","Soumise"}
		});
	}
	
	@Test
	public void requestStatus_read_dynamic_processed_true() {
		assertThat(EntityCounter.getInstance().count(RequestStatus.class, new QueryExecutorArguments().queryCountDynamic(RequestStatus.class))).isEqualTo(4l);
		QueryExecutorArguments arguments = new QueryExecutorArguments().queryReadDynamic(RequestStatus.class);
		arguments.addProjectionsFromStrings(RequestStatus.FIELD_IDENTIFIER,RequestStatus.FIELD_CODE,RequestStatus.FIELD_NAME);
		arguments.addFilterField(RequestStatusQuerier.PARAMETER_NAME_PROCESSED, Boolean.TRUE);
		assertRequestStatus((List<RequestStatus>) EntityReader.getInstance().readMany(RequestStatus.class, arguments), new Object[][] {
			{"ACCEPTEE","Accepté"},{"REJETEE","Rejeté"}
		});
	}
	
	@Test
	public void requestStatus_read_dynamic_processed_false() {
		assertThat(EntityCounter.getInstance().count(RequestStatus.class, new QueryExecutorArguments().queryCountDynamic(RequestStatus.class))).isEqualTo(4l);
		QueryExecutorArguments arguments = new QueryExecutorArguments().queryReadDynamic(RequestStatus.class);
		arguments.addProjectionsFromStrings(RequestStatus.FIELD_IDENTIFIER,RequestStatus.FIELD_CODE,RequestStatus.FIELD_NAME);
		arguments.addFilterField(RequestStatusQuerier.PARAMETER_NAME_PROCESSED, Boolean.FALSE);
		assertRequestStatus((List<RequestStatus>) EntityReader.getInstance().readMany(RequestStatus.class, arguments), new Object[][] {
			{"INITIEE","Initié"},{"SOUMISE","Soumise"}
		});
	}
	
	@Test
	public void read_dynamic() {
		assertThat(EntityCounter.getInstance().count(Request.class, new QueryExecutorArguments().queryCountDynamic(Request.class))).isEqualTo(3l);
		assertThat(EntityCounter.getInstance().count(RequestScopeFunction.class, new QueryExecutorArguments().queryCountDynamic(RequestScopeFunction.class))).isEqualTo(6l);
		QueryExecutorArguments arguments = new QueryExecutorArguments().queryReadDynamic(Request.class);
		arguments.addProjectionsFromStrings(Request.FIELD_IDENTIFIER,Request.FIELD_CODE,Request.FIELD_FIRST_NAME,Request.FIELD_LAST_NAMES,Request.FIELD_REGISTRATION_NUMBER
				,Request.FIELD_ELECTRONIC_MAIL_ADDRESS)
		.addProcessableTransientFieldsNames(Request.FIELDS_SECTION_AS_CODE_ADMINISTRATIVE_UNIT_AS_CODE_TYPE_STATUS_CREATION_DATE_PROCESSING_DATE_AS_STRINGS
				,Request.FIELDS_SCOPE_FUNCTIONS_CODES,Request.FIELDS_GRANTED_SCOPE_FUNCTIONS_CODES,Request.FIELD_DISPATCH_SLIP_CODE);
		assertRequests((List<Request>) EntityReader.getInstance().readMany(Request.class, arguments), new Object[][] {
			{"2","Komenan","Yao Christian","498721Y","kycdev@gmail.com","327","13010222","02/01/2000 à 00:00",null,"Initié",null,new String[]{"AGDTI"},null}
			,{"1","Komenan","Yao Christian","498721Y","kycdev@gmail.com","327","13010222","01/01/2000 à 00:00","02/01/2000 à 00:00","Accepté","B001",new String[]{"GDTI","AGDTI"},new String[]{"GDTI"}}			
			,{"3","Zadi","Gérard","100100A","test@mail.com","323","13010220","01/01/2000 à 00:00",null,"Initié",null,new String[]{"GDTI","OBUDGET"},null}
		});
		
		arguments = new QueryExecutorArguments().queryReadDynamic(Request.class);
		arguments.addProjectionsFromStrings(Request.FIELD_IDENTIFIER,Request.FIELD_CODE,Request.FIELD_FIRST_NAME,Request.FIELD_LAST_NAMES,Request.FIELD_REGISTRATION_NUMBER
				,Request.FIELD_ELECTRONIC_MAIL_ADDRESS)
		.addProcessableTransientFieldsNames(Request.FIELDS_SECTION_AS_CODE_ADMINISTRATIVE_UNIT_AS_CODE_TYPE_STATUS_CREATION_DATE_PROCESSING_DATE_AS_STRINGS
				,Request.FIELDS_SCOPE_FUNCTIONS_CODES_IS_CREDIT_MANAGER_HOLDER_IS_AUTHORIZING_OFFICER_HOLDER_IS_FINANCIAL_CONTROLLER_HOLDER_IS_ACCOUNTING_HOLDER,Request.FIELDS_GRANTED_SCOPE_FUNCTIONS_CODES,Request.FIELD_DISPATCH_SLIP_CODE);
		Collection<Request> requests = EntityReader.getInstance().readMany(Request.class, arguments);
		Integer index = 1;
		assertThat(CollectionHelper.getElementAt(requests, index).getIsCreditManagerHolder()).isTrue();
		assertThat(CollectionHelper.getElementAt(requests, index).getIsAuthorizingOfficerHolder()).isNull();
		assertThat(CollectionHelper.getElementAt(requests, index).getIsFinancialControllerHolder()).isNull();
		assertThat(CollectionHelper.getElementAt(requests, index).getIsAccountingHolder()).isNull();
		
		index = 0;
		assertThat(CollectionHelper.getElementAt(requests, index).getIsCreditManagerHolder()).isNull();
		assertThat(CollectionHelper.getElementAt(requests, index).getIsAuthorizingOfficerHolder()).isNull();
		assertThat(CollectionHelper.getElementAt(requests, index).getIsFinancialControllerHolder()).isNull();
		assertThat(CollectionHelper.getElementAt(requests, index).getIsAccountingHolder()).isNull();
		
		index = 2;
		assertThat(CollectionHelper.getElementAt(requests, index).getIsCreditManagerHolder()).isTrue();
		assertThat(CollectionHelper.getElementAt(requests, index).getIsAuthorizingOfficerHolder()).isTrue();
		assertThat(CollectionHelper.getElementAt(requests, index).getIsFinancialControllerHolder()).isNull();
		assertThat(CollectionHelper.getElementAt(requests, index).getIsAccountingHolder()).isNull();
	}
	
	@Test
	public void read_dynamic_sort_code_descending() {
		QueryExecutorArguments arguments = new QueryExecutorArguments().queryReadDynamic(Request.class);
		arguments.addProjectionsFromStrings(Request.FIELD_IDENTIFIER,Request.FIELD_CODE,Request.FIELD_FIRST_NAME,Request.FIELD_LAST_NAMES,Request.FIELD_REGISTRATION_NUMBER
				,Request.FIELD_ELECTRONIC_MAIL_ADDRESS)
		.addProcessableTransientFieldsNames(Request.FIELDS_SECTION_AS_CODE_ADMINISTRATIVE_UNIT_AS_CODE_TYPE_STATUS_CREATION_DATE_PROCESSING_DATE_AS_STRINGS
				,Request.FIELDS_SCOPE_FUNCTIONS_CODES,Request.FIELDS_GRANTED_SCOPE_FUNCTIONS_CODES,Request.FIELD_DISPATCH_SLIP_CODE);
		arguments.setSortOrders(Map.of("code",SortOrder.DESCENDING));
		assertRequests((List<Request>) EntityReader.getInstance().readMany(Request.class, arguments), new Object[][] {
			{"3","Zadi","Gérard","100100A","test@mail.com","323","13010220","01/01/2000 à 00:00",null,"Initié",null,new String[]{"GDTI","OBUDGET"},null}
			,{"2","Komenan","Yao Christian","498721Y","kycdev@gmail.com","327","13010222","02/01/2000 à 00:00",null,"Initié",null,new String[]{"AGDTI"},null}
			,{"1","Komenan","Yao Christian","498721Y","kycdev@gmail.com","327","13010222","01/01/2000 à 00:00","02/01/2000 à 00:00","Accepté","B001",new String[]{"GDTI","AGDTI"},new String[]{"GDTI"}}	
		});
	}
	
	@Test
	public void read_dynamic_sort_firstAndLastNames() {
		QueryExecutorArguments arguments = new QueryExecutorArguments().queryReadDynamic(Request.class);
		arguments.addProjectionsFromStrings(Request.FIELD_IDENTIFIER,Request.FIELD_CODE,Request.FIELD_FIRST_NAME,Request.FIELD_LAST_NAMES,Request.FIELD_REGISTRATION_NUMBER
				,Request.FIELD_ELECTRONIC_MAIL_ADDRESS)
		.addProcessableTransientFieldsNames(Request.FIELDS_SECTION_AS_CODE_ADMINISTRATIVE_UNIT_AS_CODE_TYPE_STATUS_CREATION_DATE_PROCESSING_DATE_AS_STRINGS
				,Request.FIELDS_SCOPE_FUNCTIONS_CODES,Request.FIELDS_GRANTED_SCOPE_FUNCTIONS_CODES,Request.FIELD_DISPATCH_SLIP_CODE);
		arguments.setSortOrders(Map.of(Request.FIELD_FIRST_NAME_AND_LAST_NAMES,SortOrder.DESCENDING));
		assertRequests((List<Request>) EntityReader.getInstance().readMany(Request.class, arguments), new Object[][] {
			{"3","Zadi","Gérard","100100A","test@mail.com","323","13010220","01/01/2000 à 00:00",null,"Initié",null,new String[]{"GDTI","OBUDGET"},null}
			,{"1","Komenan","Yao Christian","498721Y","kycdev@gmail.com","327","13010222","01/01/2000 à 00:00","02/01/2000 à 00:00","Accepté","B001",new String[]{"GDTI","AGDTI"},new String[]{"GDTI"}}
			,{"2","Komenan","Yao Christian","498721Y","kycdev@gmail.com","327","13010222","02/01/2000 à 00:00",null,"Initié",null,new String[]{"AGDTI"},null}				
		});
	}
	
	@Test
	public void read_dynamic_dispatchSlip_exist_true() {
		QueryExecutorArguments arguments = new QueryExecutorArguments().queryReadDynamic(Request.class);
		arguments.addProjectionsFromStrings(Request.FIELD_IDENTIFIER,Request.FIELD_CODE,Request.FIELD_FIRST_NAME,Request.FIELD_LAST_NAMES,Request.FIELD_REGISTRATION_NUMBER
				,Request.FIELD_ELECTRONIC_MAIL_ADDRESS)
		.addProcessableTransientFieldsNames(Request.FIELDS_SECTION_AS_CODE_ADMINISTRATIVE_UNIT_AS_CODE_TYPE_STATUS_CREATION_DATE_PROCESSING_DATE_AS_STRINGS
				,Request.FIELDS_SCOPE_FUNCTIONS_CODES,Request.FIELDS_GRANTED_SCOPE_FUNCTIONS_CODES);
		arguments.addFilterField(RequestQuerier.PARAMETER_NAME_DISPATCH_SLIP_EXISTS, Boolean.TRUE);
		assertRequests((List<Request>) EntityReader.getInstance().readMany(Request.class, arguments), new Object[][] {
			{"1","Komenan","Yao Christian","498721Y","kycdev@gmail.com","327","13010222","01/01/2000 à 00:00","02/01/2000 à 00:00","Accepté",null,new String[]{"GDTI","AGDTI"},new String[]{"GDTI"}}
		});
	}
	
	@Test
	public void read_dynamic_dispatchSlip_exist_false() {
		QueryExecutorArguments arguments = new QueryExecutorArguments().queryReadDynamic(Request.class);
		arguments.addProjectionsFromStrings(Request.FIELD_IDENTIFIER,Request.FIELD_CODE,Request.FIELD_FIRST_NAME,Request.FIELD_LAST_NAMES,Request.FIELD_REGISTRATION_NUMBER
				,Request.FIELD_ELECTRONIC_MAIL_ADDRESS)
		.addProcessableTransientFieldsNames(Request.FIELDS_SECTION_AS_CODE_ADMINISTRATIVE_UNIT_AS_CODE_TYPE_STATUS_CREATION_DATE_PROCESSING_DATE_AS_STRINGS
				,Request.FIELDS_SCOPE_FUNCTIONS_CODES,Request.FIELDS_GRANTED_SCOPE_FUNCTIONS_CODES);
		arguments.addFilterField(RequestQuerier.PARAMETER_NAME_DISPATCH_SLIP_EXISTS, Boolean.FALSE);
		assertRequests((List<Request>) EntityReader.getInstance().readMany(Request.class, arguments), new Object[][] {
			{"2","Komenan","Yao Christian","498721Y","kycdev@gmail.com","327","13010222","02/01/2000 à 00:00",null,"Initié",null,new String[]{"AGDTI"},null}
			,{"3","Zadi","Gérard","100100A","test@mail.com","323","13010220","01/01/2000 à 00:00",null,"Initié",null,new String[]{"GDTI","OBUDGET"},null}
		});
	}
	
	@Test
	public void read_dynamic_processed_true() {
		QueryExecutorArguments arguments = new QueryExecutorArguments().queryReadDynamic(Request.class);
		arguments.addProjectionsFromStrings(Request.FIELD_IDENTIFIER,Request.FIELD_CODE,Request.FIELD_FIRST_NAME,Request.FIELD_LAST_NAMES,Request.FIELD_REGISTRATION_NUMBER
				,Request.FIELD_ELECTRONIC_MAIL_ADDRESS)
		.addProcessableTransientFieldsNames(Request.FIELDS_SECTION_AS_CODE_ADMINISTRATIVE_UNIT_AS_CODE_TYPE_STATUS_CREATION_DATE_PROCESSING_DATE_AS_STRINGS
				,Request.FIELDS_SCOPE_FUNCTIONS_CODES,Request.FIELDS_GRANTED_SCOPE_FUNCTIONS_CODES);
		arguments.addFilterField(RequestQuerier.PARAMETER_NAME_PROCESSED, Boolean.TRUE);
		assertRequests((List<Request>) EntityReader.getInstance().readMany(Request.class, arguments), new Object[][] {
			{"1","Komenan","Yao Christian","498721Y","kycdev@gmail.com","327","13010222","01/01/2000 à 00:00","02/01/2000 à 00:00","Accepté",null,new String[]{"GDTI","AGDTI"},new String[]{"GDTI"}}
		});
	}
	
	@Test
	public void read_dynamic_processed_false() {
		QueryExecutorArguments arguments = new QueryExecutorArguments().queryReadDynamic(Request.class);
		arguments.addProjectionsFromStrings(Request.FIELD_IDENTIFIER,Request.FIELD_CODE,Request.FIELD_FIRST_NAME,Request.FIELD_LAST_NAMES,Request.FIELD_REGISTRATION_NUMBER
				,Request.FIELD_ELECTRONIC_MAIL_ADDRESS)
		.addProcessableTransientFieldsNames(Request.FIELDS_SECTION_AS_CODE_ADMINISTRATIVE_UNIT_AS_CODE_TYPE_STATUS_CREATION_DATE_PROCESSING_DATE_AS_STRINGS
				,Request.FIELDS_SCOPE_FUNCTIONS_CODES,Request.FIELDS_GRANTED_SCOPE_FUNCTIONS_CODES);
		arguments.addFilterField(RequestQuerier.PARAMETER_NAME_PROCESSED, Boolean.FALSE);
		assertRequests((List<Request>) EntityReader.getInstance().readMany(Request.class, arguments), new Object[][] {
			{"2","Komenan","Yao Christian","498721Y","kycdev@gmail.com","327","13010222","02/01/2000 à 00:00",null,"Initié",null,new String[]{"AGDTI"},null}
			,{"3","Zadi","Gérard","100100A","test@mail.com","323","13010220","01/01/2000 à 00:00",null,"Initié",null,new String[]{"GDTI","OBUDGET"},null}
		});
	}
	
	@Test
	public void read_dynamic_administrative_unit_DTI() {
		QueryExecutorArguments arguments = new QueryExecutorArguments().queryReadDynamic(Request.class);
		arguments.addProjectionsFromStrings(Request.FIELD_IDENTIFIER,Request.FIELD_CODE,Request.FIELD_FIRST_NAME,Request.FIELD_LAST_NAMES,Request.FIELD_REGISTRATION_NUMBER
				,Request.FIELD_ELECTRONIC_MAIL_ADDRESS)
		.addProcessableTransientFieldsNames(Request.FIELDS_SECTION_ADMINISTRATIVE_UNIT_TYPE_STATUS_CREATION_DATE_PROCESSING_DATE_AS_STRINGS
				,Request.FIELDS_SCOPE_FUNCTIONS_CODES,Request.FIELDS_GRANTED_SCOPE_FUNCTIONS_CODES);
		arguments.addFilterField(RequestQuerier.PARAMETER_NAME_ADMINISTRATIVE_UNITS_IDENTIFIERS, List.of("DTI"));
		assertRequests((List<Request>) EntityReader.getInstance().readMany(Request.class, arguments), new Object[][] {
			{"2","Komenan","Yao Christian","498721Y","kycdev@gmail.com","327 Budget","13010222 DTI","02/01/2000 à 00:00",null,"Initié",null,new String[]{"AGDTI"},null}
			,{"1","Komenan","Yao Christian","498721Y","kycdev@gmail.com","327 Budget","13010222 DTI","01/01/2000 à 00:00","02/01/2000 à 00:00","Accepté",null,new String[]{"GDTI","AGDTI"},new String[]{"GDTI"}}			
		});
	}
	
	@Test
	public void read_dynamic_administrative_unit_DGDDL() {
		QueryExecutorArguments arguments = new QueryExecutorArguments().queryReadDynamic(Request.class);
		arguments.addProjectionsFromStrings(Request.FIELD_IDENTIFIER,Request.FIELD_CODE,Request.FIELD_FIRST_NAME,Request.FIELD_LAST_NAMES,Request.FIELD_REGISTRATION_NUMBER
				,Request.FIELD_ELECTRONIC_MAIL_ADDRESS)
		.addProcessableTransientFieldsNames(Request.FIELDS_SECTION_AS_CODE_ADMINISTRATIVE_UNIT_AS_CODE_TYPE_STATUS_CREATION_DATE_PROCESSING_DATE_AS_STRINGS
				,Request.FIELDS_SCOPE_FUNCTIONS_CODES,Request.FIELDS_GRANTED_SCOPE_FUNCTIONS_CODES);
		arguments.addFilterField(RequestQuerier.PARAMETER_NAME_ADMINISTRATIVE_UNITS_IDENTIFIERS, List.of("DGDDL"));
		assertRequests((List<Request>) EntityReader.getInstance().readMany(Request.class, arguments), new Object[][] {
			{"3","Zadi","Gérard","100100A","test@mail.com","323","13010220","01/01/2000 à 00:00",null,"Initié",null,new String[]{"GDTI","OBUDGET"},null}
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
			{"2","Komenan","Yao Christian","498721Y","kycdev@gmail.com","327 Budget","13010222 DTI","02/01/2000 à 00:00",null,"Initié",null,new String[]{"AGDTI"},null}
			,{"1","Komenan","Yao Christian","498721Y","kycdev@gmail.com","327 Budget","13010222 DTI","01/01/2000 à 00:00","02/01/2000 à 00:00","Accepté",null,new String[]{"GDTI","AGDTI"},new String[]{"GDTI"}}
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
			{"3","Zadi","Gérard","100100A","test@mail.com","323 Intérieur","13010220 DGDDL","01/01/2000 à 00:00",null,"Initié",null,new String[]{"GDTI","OBUDGET"},null}
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
			{"1","Komenan","Yao Christian","498721Y","kycdev@gmail.com","327 Budget","13010222 DTI","01/01/2000 à 00:00","02/01/2000 à 00:00","Accepté",null,new String[]{"GDTI","AGDTI"},new String[]{"GDTI"}}
			,{"3","Zadi","Gérard","100100A","test@mail.com","323 Intérieur","13010220 DGDDL","01/01/2000 à 00:00",null,"Initié",null,new String[]{"GDTI","OBUDGET"},null}
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
			{"1","Komenan","Yao Christian","498721Y","kycdev@gmail.com","327 Budget","13010222 DTI","01/01/2000 à 00:00","02/01/2000 à 00:00","Accepté",null,new String[]{"GDTI","AGDTI"},new String[]{"GDTI"}}
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
			{"2","Komenan","Yao Christian","498721Y","kycdev@gmail.com","327 Budget","13010222 DTI","02/01/2000 à 00:00",null,"Initié",null,new String[]{"AGDTI"},null}
			,{"1","Komenan","Yao Christian","498721Y","kycdev@gmail.com","327 Budget","13010222 DTI","01/01/2000 à 00:00","02/01/2000 à 00:00","Accepté",null,new String[]{"GDTI","AGDTI"},new String[]{"GDTI"}}		
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
			{"3","Zadi","Gérard","100100A","test@mail.com","323 Intérieur","13010220 DGDDL","01/01/2000 à 00:00",null,"Initié",null,new String[]{"GDTI","OBUDGET"},null}
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
			{"2","Komenan","Yao Christian","498721Y","kycdev@gmail.com","327 Budget","13010222 DTI","02/01/2000 à 00:00",null,"Initié",null,new String[]{"AGDTI"},null}
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
			{"1","Komenan","Yao Christian","498721Y","kycdev@gmail.com","327 Budget","13010222 DTI","01/01/2000 à 00:00","02/01/2000 à 00:00","Accepté",null,new String[]{"GDTI","AGDTI"},new String[]{"GDTI"}}
			,{"3","Zadi","Gérard","100100A","test@mail.com","323 Intérieur","13010220 DGDDL","01/01/2000 à 00:00",null,"Initié",null,new String[]{"GDTI","OBUDGET"},null}
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
			{"2","Komenan","Yao Christian","498721Y","kycdev@gmail.com","327 Budget","13010222 DTI","02/01/2000 à 00:00",null,"Initié",null,new String[]{"AGDTI"},null}
			,{"3","Zadi","Gérard","100100A","test@mail.com","323 Intérieur","13010220 DGDDL","01/01/2000 à 00:00",null,"Initié",null,new String[]{"GDTI","OBUDGET"},null}
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
			{"1","Komenan","Yao Christian","498721Y","kycdev@gmail.com","327 Budget","13010222 DTI","01/01/2000 à 00:00","02/01/2000 à 00:00","Accepté",null,new String[]{"GDTI","AGDTI"},new String[]{"GDTI"}}
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
			{"2","Komenan","Yao Christian","498721Y","kycdev@gmail.com","327 Budget","13010222 DTI","02/01/2000 à 00:00",null,"Initié",null,new String[]{"AGDTI"},null}
		});
	}
	
	@Test
	public void readForSendSignaturesSpecimensByElectronicMailAddress() {
		Object[] array = RequestQuerier.getInstance().readForSendSignaturesSpecimensByElectronicMailAddress("kycdev@gmail.com");
		assertThat(array).isNotNull();
		assertThat(array[0]).isEqualTo("1");
		assertThat(array[1]).isEqualTo("kycdev@gmail.com");
		assertThat(array[2]).isEqualTo("Demande de poste budgétaire");
		assertThat(array[3]).isEqualTo(null);
		assertThat(array[4]).isEqualTo("Komenan");
		assertThat(array[5]).isEqualTo("Yao Christian");
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
				assertThat(request.getDispatchSlipAsString()).as("Bordereau").isEqualTo(expectedStrings[index][10]);
				
				if(CollectionHelper.isEmpty(request.getScopeFunctionsCodes()))
					assertThat(expectedStrings[index][11]).as("Postes demandées").isNull();
				else
					assertThat(request.getScopeFunctionsCodes()).as("Postes demandées").containsOnly((String[])expectedStrings[index][11]);
				
				if(CollectionHelper.isEmpty(request.getGrantedScopeFunctionsCodes()))
					assertThat(expectedStrings[index][12]).as("Postes accordées").isNull();
				else
					assertThat(request.getGrantedScopeFunctionsCodes()).as("Postes accordées").containsOnly((String[])expectedStrings[index][12]);
			}
		}
	}
	
	public static void assertRequestStatus(List<RequestStatus> requestStatus,Object[][] expectedStrings) {
		if(CollectionHelper.isEmpty(requestStatus))
			assertThat(expectedStrings).isNull();
		else {
			assertThat(expectedStrings).as("nombre de statuts").hasSize(requestStatus.size());
			for(Integer index = 0; index < requestStatus.size(); index = index + 1) {
				RequestStatus requestStatus_ = requestStatus.get(index);
				assertThat(requestStatus_.getCode()).as("Code").isEqualTo(expectedStrings[index][0]);
				assertThat(requestStatus_.getName()).as("Libelle").isEqualTo(expectedStrings[index][1]);
			}
		}
	}
	
	public static void assertRequestDispatchSlip(List<RequestDispatchSlip> requestDispatchSlips,Object[][] expectedStrings) {
		if(CollectionHelper.isEmpty(requestDispatchSlips))
			assertThat(expectedStrings).isNull();
		else {
			assertThat(expectedStrings).as("nombre de bordereaux").hasSize(requestDispatchSlips.size());
			for(Integer index = 0; index < requestDispatchSlips.size(); index = index + 1) {
				RequestDispatchSlip requestDispatchSlip = requestDispatchSlips.get(index);
				assertThat(requestDispatchSlip.getCode()).as("Code").isEqualTo(expectedStrings[index][0]);
				assertThat(requestDispatchSlip.getName()).as("Libelle").isEqualTo(expectedStrings[index][1]);
				assertThat(requestDispatchSlip.getSection()).as("Section").isNotNull();
				assertThat(requestDispatchSlip.getSection().getCode()).as("Code section").isEqualTo(expectedStrings[index][2]);	
				assertThat(requestDispatchSlip.getFunction()).as("Fonction").isNotNull();
				assertThat(requestDispatchSlip.getFunction().getCode()).as("Code fonction").isEqualTo(expectedStrings[index][3]);	
				assertThat(requestDispatchSlip.getNumberOfRequests()).as("Nombre de demandes").isEqualTo(expectedStrings[index][4]);
			}
		}
	}
	
	public static void assertRequestDispatchSlipAsStrings(List<RequestDispatchSlip> requestDispatchSlips,Object[][] expectedStrings) {
		if(CollectionHelper.isEmpty(requestDispatchSlips))
			assertThat(expectedStrings).isNull();
		else {
			assertThat(expectedStrings).as("nombre de bordereaux").hasSize(requestDispatchSlips.size());
			for(Integer index = 0; index < requestDispatchSlips.size(); index = index + 1) {
				RequestDispatchSlip requestDispatchSlip = requestDispatchSlips.get(index);
				assertThat(requestDispatchSlip.getCode()).as("Code").isEqualTo(expectedStrings[index][0]);
				assertThat(requestDispatchSlip.getName()).as("Libelle").isEqualTo(expectedStrings[index][1]);
				assertThat(requestDispatchSlip.getSectionAsString()).as("Section").isEqualTo(expectedStrings[index][2]);	
				assertThat(requestDispatchSlip.getFunctionAsString()).as("Fonction").isEqualTo(expectedStrings[index][3]);	
				assertThat(requestDispatchSlip.getNumberOfRequests()).as("Nombre de demandes").isEqualTo(expectedStrings[index][4]);
			}
		}
	}
}