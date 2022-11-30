package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import static org.cyk.utility.persistence.query.Language.jpql;
import static org.cyk.utility.persistence.query.Language.parenthesis;
import static org.cyk.utility.persistence.query.Language.Order.desc;
import static org.cyk.utility.persistence.query.Language.Order.order;
import static org.cyk.utility.persistence.query.Language.Select.concat;
import static org.cyk.utility.persistence.query.Language.Select.concatCodeName;
import static org.cyk.utility.persistence.query.Language.Select.fields;
import static org.cyk.utility.persistence.query.Language.Select.select;
import static org.cyk.utility.persistence.query.Language.Where.and;
import static org.cyk.utility.persistence.query.Language.Where.like;
import static org.cyk.utility.persistence.query.Language.Where.or;
import static org.cyk.utility.persistence.query.Language.Where.where;

import java.io.Serializable;
import java.util.Collection;
import java.util.Date;

import javax.persistence.EntityManager;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.value.Value;
import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.persistence.query.QueryManager;
import org.cyk.utility.persistence.query.QueryName;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Request;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestStatus;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestType;

public interface RequestQuerier extends Querier {

	Integer NUMBER_OF_WORDS = 6;
	
	String PARAMETER_NAME_SEARCH = "search";
	
	String PARAMETER_NAME_ELECTRONIC_MAIL_ADDRESS = "electronicMailAddress";
	String PARAMETER_NAME_ACCESS_TOKEN = "accessToken";
	String PARAMETER_NAME_ACTOR_IDENTIFIER = "actorIdentifier";
	String PARAMETER_NAME_ACTOR_IDENTIFIER_NULLABLE = PARAMETER_NAME_ACTOR_IDENTIFIER+"Nullable";	
	String PARAMETER_NAME_TYPE_IDENTIFIER = "typeIdentifier";

	String PARAMETER_NAME_STATUS_IDENTIFIER = "statusIdentifier";
	String PARAMETER_NAME_STATUS_IDENTIFIER_NULLABLE = PARAMETER_NAME_STATUS_IDENTIFIER+"Nullable";
	
	String PARAMETER_NAME_REGISTRATION_NUMBER = "registrationNumber";
	String PARAMETER_NAME_REGISTRATION_NUMBER_NULLABLE = PARAMETER_NAME_REGISTRATION_NUMBER+"Nullable";
	
	String PARAMETER_NAME_FIRST_NAME = "firstName";
	String PARAMETER_NAME_LAST_NAMES = "lastNames";
	
	String PARAMETER_NAME_MOBILE_PHONE_NUMBER = "mobilePhoneNumber";
	
	String PARAMETER_NAME_SECTION_IDENTIFIER = "sectionIdentifier";
	String PARAMETER_NAME_SECTIONS_IDENTIFIERS = "sectionsIdentifiers";
	
	String PARAMETER_NAME_ADMINISTRATIVE_UNIT = "administrativeunit";	
	String PARAMETER_NAME_ADMINISTRATIVE_UNIT_IDENTIFIER = "administrativeunitIdentifier";
	String PARAMETER_NAME_ADMINISTRATIVE_UNITS_IDENTIFIERS = "administrativeUnitsIdentifiers";
	String PARAMETER_NAME_ADMINISTRATIVE_UNITS_SECTIONS_IDENTIFIERS = "administrativeUnitsSectionsIdentifiers";
	String PARAMETER_NAME_ADMINISTRATIVE_UNIT_SECTION_IDENTIFIER = PARAMETER_NAME_ADMINISTRATIVE_UNIT+"SectionIdentifier";
	String PARAMETER_NAME_ADMINISTRATIVE_UNIT_SECTION_IDENTIFIER_NULLABLE = PARAMETER_NAME_ADMINISTRATIVE_UNIT_SECTION_IDENTIFIER+"Nullable";
	
	String PARAMETER_NAME_FUNCTION_IDENTIFIER = "functionIdentifier";
	String PARAMETER_NAME_FUNCTIONS_IDENTIFIERS = "functionsIdentifiers";
	String PARAMETER_NAME_FUNCTION_IDENTIFIER_NULLABLE = PARAMETER_NAME_FUNCTION_IDENTIFIER+"Nullable";
	
	String PARAMETER_NAME_BUDGET_CATEGORY_IDENTIFIER = "budgetCategoryIdentifier";
	String PARAMETER_NAME_BUDGET_CATEGORIES_IDENTIFIERS = "budgetCategoriesIdentifiers";
	String PARAMETER_NAME_BUDGET_CATEGORY_IDENTIFIER_NULLABLE = PARAMETER_NAME_BUDGET_CATEGORY_IDENTIFIER+"Nullable";
	
	String PARAMETER_NAME_STATUS_IDENTIFIERS = "statusIdentifiers";
	String PARAMETER_NAME_TYPES_IDENTIFIERS = "typesIdentifiers";
	String PARAMETER_NAME_DISPATCH_SLIP_IDENTIFIERS = "dispatchSlipIdentifiers";
	String PARAMETER_NAME_DISPATCH_SLIP_EXISTS = "dispatchSlipExists";
	
	String PARAMETER_NAME_LOWEST_CREATION_DATE = "lowestCreationDate";
	String PARAMETER_NAME_HIGHEST_CREATION_DATE = "highestCreationDate";
	String PARAMETER_NAME_LOWEST_PROCESSING_DATE = "lowestProcessingDate";
	String PARAMETER_NAME_HIGHEST_PROCESSING_DATE = "highestProcessingDate";
	
	String PARAMETER_NAME_DISPATCH_SLIP_IDENTIFIER = "dispatchSlipIdentifier";
	String PARAMETER_NAME_DISPATCH_SLIP_IDENTIFIER_NULLABLE = PARAMETER_NAME_DISPATCH_SLIP_IDENTIFIER+"Nullable";
	String PARAMETER_NAME_DISPATCH_SLIP_IS_NULL = "dispatchSlipIsNull";
	String PARAMETER_NAME_DISPATCH_SLIP_IS_NULL_NULLABLE = PARAMETER_NAME_DISPATCH_SLIP_IS_NULL+"Nullable";
	String PARAMETER_NAME_DISPATCH_SLIP_IS_NOT_NULL = "dispatchSlipIsNotNull";
	String PARAMETER_NAME_DISPATCH_SLIP_IS_NOT_NULL_NULLABLE = PARAMETER_NAME_DISPATCH_SLIP_IS_NOT_NULL+"Nullable";
	
	String PARAMETER_NAME_PROCESSING_DATE_IS_NULL = "processingDateIsNull";
	String PARAMETER_NAME_PROCESSING_DATE_IS_NULL_NULLABLE = PARAMETER_NAME_PROCESSING_DATE_IS_NULL+"Nullable";
	String PARAMETER_NAME_PROCESSING_DATE_IS_NOT_NULL = "processingDateIsNotNull";
	String PARAMETER_NAME_PROCESSING_DATE_IS_NOT_NULL_NULLABLE = PARAMETER_NAME_PROCESSING_DATE_IS_NOT_NULL+"Nullable";
	String PARAMETER_NAME_REQUEST_DISPATCH_SLIP_IDENTIFIER = "requestDispatchSlipIdentifier";
	
	String PARAMETER_NAME_PROCESSED = "processed";
	String PARAMETER_NAME_ACCEPTED = "accepted";
	
	Request readOne(QueryExecutorArguments arguments);
	Collection<Request> readMany(QueryExecutorArguments arguments);
	Long count(QueryExecutorArguments arguments);
	
	String QUERY_IDENTIFIER_READ_WHERE_FILTER = QueryIdentifierBuilder.getInstance().build(Request.class, QueryName.READ_WHERE_FILTER.getValue());
	Collection<Request> readWhereFilter(QueryExecutorArguments arguments);
	
	String QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_UI = QUERY_IDENTIFIER_READ_WHERE_FILTER+"ForUI";
	Collection<Request> readWhereFilterForUI(QueryExecutorArguments arguments);
	
	String QUERY_IDENTIFIER_COUNT_WHERE_FILTER = QueryIdentifierBuilder.getInstance().build(Request.class, QueryName.COUNT_WHERE_FILTER.getValue());
	Long countWhereFilter(QueryExecutorArguments arguments);
	
	String QUERY_IDENTIFIER_INSTANTIATE_ONE_BY_TYPE_IDENTIFIER = QueryIdentifierBuilder.getInstance().build(Request.class, "instantiateOneByTypeIdentifier");
	Request instantiateOneByTypeIdentifier(String typeIdentifier);
	
	String QUERY_IDENTIFIER_INSTANTIATE_ONE_BY_TYPE_IDENTIFIER_BY_ELECTRONIC_MAIL_ADDRESS = QueryIdentifierBuilder.getInstance().build(Request.class, "instantiateOneByTypeIdentifierByElectronicMailAddress");
	Request instantiateOneByTypeIdentifierByElectronicMailAddress(String typeIdentifier,String electronicMailAddress);
	
	String QUERY_IDENTIFIER_INSTANTIATE_ONE_BY_TYPE_IDENTIFIER_BY_ACTOR_IDENTIFIER = QueryIdentifierBuilder.getInstance().build(Request.class, "instantiateOneByTypeIdentifierByActorIdentifier");
	Request instantiateOneByTypeIdentifierByActorIdentifier(String typeIdentifier,String actorIdentifier);
	
	String QUERY_IDENTIFIER_READ_BY_IDENTIFIER = QueryIdentifierBuilder.getInstance().build(Request.class, "readByIdentifier");
	Request readByIdentifier(String identifier);
	
	String QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_UI = QueryIdentifierBuilder.getInstance().build(Request.class, "readByIdentifierForUI");
	Request readByIdentifierForUI(QueryExecutorArguments arguments);
	
	String QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_EDIT = QueryIdentifierBuilder.getInstance().build(Request.class, "readByIdentifierForEdit");
	Request readByIdentifierForEdit(String identifier);
	
	String QUERY_IDENTIFIER_READ_BY_ACCESS_TOKEN = QueryIdentifierBuilder.getInstance().build(Request.class, "readByAccessToken");
	Request readByAccessToken(String accessToken);
	
	String QUERY_IDENTIFIER_READ_BY_ACCESS_TOKEN_FOR_UI = QueryIdentifierBuilder.getInstance().build(Request.class, "readByAccessTokenForUI");
	Request readByAccessTokenForUI(String accessToken);
	
	String QUERY_IDENTIFIER_READ_BY_ELECTRONIC_MAIL_ADDRESS = QueryIdentifierBuilder.getInstance().build(Request.class, "readByElectronicMailAddress");
	Collection<Request> readByElectronicMailAddress(String electronicMailAddress);
	
	String QUERY_IDENTIFIER_READ_BY_DISPATCH_SLIP_IDENTIFIER = QueryIdentifierBuilder.getInstance().build(Request.class, "readByDispatchSlipIdentifier");
	Collection<Request> readByDispatchSlipIdentifier(String dispatchSlipIdentifier);
	
	String QUERY_IDENTIFIER_READ_BY_DISPATCH_SLIP_IDENTIFIER_FOR_UI = QueryIdentifierBuilder.getInstance().build(Request.class
			, QUERY_IDENTIFIER_READ_BY_DISPATCH_SLIP_IDENTIFIER+"ForUI");
	Collection<Request> readByDispatchSlipIdentifierForUI(String dispatchSlipIdentifier);
	
	String QUERY_IDENTIFIER_READ_PHOTO_BY_IDENTIFIER = QueryIdentifierBuilder.getInstance().build(Request.class, "readPhotoByIdentifier");
	byte[] readPhotoByIdentifier(String identifier);
	
	String QUERY_IDENTIFIER_READ_ACT_OF_APPOINTMENT_BY_IDENTIFIER = QueryIdentifierBuilder.getInstance().build(Request.class, "readActOfAppointmentByIdentifier");
	byte[] readActOfAppointmentByIdentifier(String identifier);
	
	String QUERY_IDENTIFIER_READ_SIGNATURE_BY_IDENTIFIER = QueryIdentifierBuilder.getInstance().build(Request.class, "readSignatureByIdentifier");
	byte[] readSignatureByIdentifier(String identifier);
	
	String QUERY_IDENTIFIER_READ_SIGNED_REQUEST_SHEET_BY_IDENTIFIER = QueryIdentifierBuilder.getInstance().build(Request.class, "readSignedRequestSheetByIdentifier");
	byte[] readSignedRequestSheetByIdentifier(String identifier);
	
	void exportForAccountCreation(String actor,String functionality,String action,Date date,EntityManager entityManager);
	
	Collection<Object[]> readFromViewByElectronicMailAddresses(Collection<String> emails);
	Collection<Object[]> readFromViewByElectronicMailAddresses(String...emails);
	Collection<Object[]> readFromViewByRequests(Collection<Request> requests);
	Collection<Object[]> readFromViewByRequests(Request...requests);
	
	Object[] readForSendSignaturesSpecimensByElectronicMailAddress(String electronicMailAddress);
	
	String QUERY_IDENTIFIER_READ_DYNAMIC = QueryIdentifierBuilder.getInstance().build(Request.class, QueryName.READ_DYNAMIC);	
	String QUERY_IDENTIFIER_READ_DYNAMIC_ONE = QueryIdentifierBuilder.getInstance().build(Request.class, QueryName.READ_DYNAMIC_ONE);
	String QUERY_IDENTIFIER_COUNT_DYNAMIC = QueryIdentifierBuilder.getInstance().build(Request.class, QueryName.COUNT_DYNAMIC);
	/*
	String QUERY_IDENTIFIER_COUNT_FOR_EACH_YEAR = "Request.countForEachYear";
	Object[][] countForEachYear(QueryExecutorArguments arguments);
	String QUERY_IDENTIFIER_COUNT_FOR_EACH_YEAR_FOR_EACH_MONTH = "Request.countForEachYearForEachMonth";
	Object[][] countForEachYearForEachMonth(QueryExecutorArguments arguments);
	String QUERY_IDENTIFIER_COUNT_FOR_EACH_YEAR_FOR_EACH_MONTH_FOR_EACH_DAY = "Request.countForEachYearForEachMonthForEachDay";
	Object[][] countForEachYearForEachMonthForEachDay(QueryExecutorArguments arguments);
	*/
	public static abstract class AbstractImpl extends Querier.AbstractImpl implements RequestQuerier,Serializable {
		
	}
	
	/**/
	
	/**/
	
	static RequestQuerier getInstance() {
		return Helper.getInstance(RequestQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		QueryManager.getInstance().register(
			Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_WHERE_FILTER
			,Query.FIELD_TUPLE_CLASS,Request.class,Query.FIELD_RESULT_CLASS,Request.class
			,Query.FIELD_VALUE,getReadWhereFilter()
			).setTupleFieldsNamesIndexesFromFieldsNames(Request.FIELD_IDENTIFIER,Request.FIELD_CODE,Request.FIELD_FIRST_NAME,Request.FIELD_LAST_NAMES
					,Request.FIELD_REGISTRATION_NUMBER,Request.FIELD_ELECTRONIC_MAIL_ADDRESS,Request.FIELD_MOBILE_PHONE_NUMBER,Request.FIELD_STATUS
					,Request.FIELD_CREATION_DATE_AS_STRING,Request.FIELD_PROCESSING_DATE_AS_STRING,Request.FIELD_ACCOUNT_CREATION_DATE,Request.FIELD_ACCOUNT_CREATION_MESSAGE
					,Request.FIELD_ACTOR_CODE,Request.FIELD_ACTOR_NAMES,Request.FIELD_TYPE_AS_STRING,Request.FIELD_STATUS_AS_STRING
					,Request.FIELD_ADMINISTRATIVE_UNIT_AS_STRING,Request.FIELD_SECTION_AS_STRING)
				
			,Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_WHERE_FILTER
			,Query.FIELD_TUPLE_CLASS,Request.class,Query.FIELD_RESULT_CLASS,Long.class
			,Query.FIELD_VALUE,jpql("SELECT COUNT(t.identifier)",getReadWhereFilterFromWhere())
			)
			
			,Query.buildSelect(Request.class, QUERY_IDENTIFIER_READ_BY_IDENTIFIER, "SELECT t FROM Request t WHERE t.identifier = :"+PARAMETER_NAME_IDENTIFIER)
			,Query.buildSelect(Request.class, QUERY_IDENTIFIER_READ_BY_ACCESS_TOKEN, "SELECT t FROM Request t WHERE t.accessToken = :"+PARAMETER_NAME_ACCESS_TOKEN)
			,Query.buildSelect(Request.class, QUERY_IDENTIFIER_READ_BY_ELECTRONIC_MAIL_ADDRESS, "SELECT t FROM Request t WHERE t.electronicMailAddress = :"+PARAMETER_NAME_ELECTRONIC_MAIL_ADDRESS)
			/*,Query.buildSelect(Request.class, QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_UI
					, jpql(select("t")
							,from("FROM Request t")
							,where("t.identifier = :"+PARAMETER_NAME_IDENTIFIER))
					)*/
			
			,Query.buildSelect(Request.class, QUERY_IDENTIFIER_READ_BY_DISPATCH_SLIP_IDENTIFIER, "SELECT t FROM Request t WHERE t.dispatchSlip.identifier = :"+PARAMETER_NAME_REQUEST_DISPATCH_SLIP_IDENTIFIER)
			,Query.buildSelect(Request.class, QUERY_IDENTIFIER_READ_BY_DISPATCH_SLIP_IDENTIFIER_FOR_UI, "SELECT t FROM Request t WHERE t.dispatchSlip.identifier = :"
					+PARAMETER_NAME_REQUEST_DISPATCH_SLIP_IDENTIFIER)
			
			,Query.buildSelect(Request.class, QUERY_IDENTIFIER_READ_PHOTO_BY_IDENTIFIER, "SELECT t.photo FROM Request t WHERE t.identifier = :"+PARAMETER_NAME_IDENTIFIER)
				.setTupleFieldsNamesIndexesFromFieldsNames(Request.FIELD_PHOTO)
			,Query.buildSelect(Request.class, QUERY_IDENTIFIER_READ_ACT_OF_APPOINTMENT_BY_IDENTIFIER, "SELECT t.actOfAppointment FROM Request t WHERE t.identifier = :"+PARAMETER_NAME_IDENTIFIER)
				.setTupleFieldsNamesIndexesFromFieldsNames(Request.FIELD_PHOTO)
			,Query.buildSelect(Request.class, QUERY_IDENTIFIER_READ_SIGNATURE_BY_IDENTIFIER, "SELECT t.signature FROM Request t WHERE t.identifier = :"+PARAMETER_NAME_IDENTIFIER)
				.setTupleFieldsNamesIndexesFromFieldsNames(Request.FIELD_SIGNATURE)
				,Query.buildSelect(Request.class, QUERY_IDENTIFIER_READ_SIGNED_REQUEST_SHEET_BY_IDENTIFIER, "SELECT t.signedRequestSheet FROM Request t WHERE t.identifier = :"+PARAMETER_NAME_IDENTIFIER)
				.setTupleFieldsNamesIndexesFromFieldsNames(Request.FIELD_SIGNATURE)
			//,new Query().setIdentifier(QUERY_IDENTIFIER_COUNT_FOR_EACH_YEAR).setValue("SELECT t.identifier FROM Request t GROUP BY YEAR(t.creationDate)")
		);
	}
	
	static String getReadWhereFilter() {
		return jpql(select(
				fields("t",Request.FIELD_IDENTIFIER,Request.FIELD_CODE,Request.FIELD_FIRST_NAME,Request.FIELD_LAST_NAMES,Request.FIELD_REGISTRATION_NUMBER
						,Request.FIELD_ELECTRONIC_MAIL_ADDRESS,Request.FIELD_MOBILE_PHONE_NUMBER,Request.FIELD_STATUS
						,Request.FIELD_CREATION_DATE,Request.FIELD_PROCESSING_DATE,Request.FIELD_ACCOUNT_CREATION_DATE,Request.FIELD_ACCOUNT_CREATION_MESSAGE)
				,fields("a",Actor.FIELD_CODE),concat("t", Actor.FIELD_FIRST_NAME,Actor.FIELD_LAST_NAMES) 
				,fields("rt",RequestType.FIELD_NAME),fields("rs",RequestStatus.FIELD_NAME),concatCodeName("au"),concatCodeName("section")
				)
				,getReadWhereFilterFromWhere(),getOrderBy());
	}
	
	static String getReadWhereFilterFromWhere() {
		return jpql(
				"FROM Request t"
				,"LEFT JOIN Actor a ON a = t.actor"
				,"LEFT JOIN RequestType rt ON rt = t.type"
				,"LEFT JOIN RequestStatus rs ON rs = t.status"
				,"LEFT JOIN AdministrativeUnit au ON au = t.administrativeUnit"
				,"LEFT JOIN Section section ON section = au.section"
				,"LEFT JOIN RequestDispatchSlip rds ON rds = t.dispatchSlip"
				,getReadWhereFilterWhere()
			);
	}
	
	static String getReadWhereFilterWhere() {
		return where(and(
				"t.identifier NOT IN :"+PARAMETER_NAME_EXCLUDED_IDENTIFIERS
				/* Actor */
				,parenthesis(or(String.format(":%s = true", PARAMETER_NAME_ACTOR_IDENTIFIER_NULLABLE),"a.identifier = :"+PARAMETER_NAME_ACTOR_IDENTIFIER))
				/* Status */
				,parenthesis(or(String.format(":%s = true", PARAMETER_NAME_STATUS_IDENTIFIER_NULLABLE),"rs.identifier = :"+PARAMETER_NAME_STATUS_IDENTIFIER))
				/* */
				/*
				,parenthesis(or(
						like("au", AdministrativeUnit.FIELD_CODE, PARAMETER_NAME_ADMINISTRATIVE_UNIT)
						,like("au", AdministrativeUnit.FIELD_NAME, PARAMETER_NAME_ADMINISTRATIVE_UNIT,NUMBER_OF_WORDS)
						))
				*/
				,parenthesis(like("t", Request.FIELD_CODE, PARAMETER_NAME_CODE))
				,parenthesis(like("t", Request.FIELD_FIRST_NAME, PARAMETER_NAME_FIRST_NAME))
				,parenthesis(like("t", Request.FIELD_LAST_NAMES, PARAMETER_NAME_LAST_NAMES,NUMBER_OF_WORDS))
				,parenthesis(or(String.format(":%s = true",PARAMETER_NAME_REGISTRATION_NUMBER_NULLABLE)
						,like("t", Request.FIELD_REGISTRATION_NUMBER, PARAMETER_NAME_REGISTRATION_NUMBER)))
				,parenthesis(like("t", Request.FIELD_ELECTRONIC_MAIL_ADDRESS, PARAMETER_NAME_ELECTRONIC_MAIL_ADDRESS))
				
				/* Administrative Unit Section */
				,parenthesis(or(String.format(":%s = true", PARAMETER_NAME_ADMINISTRATIVE_UNIT_SECTION_IDENTIFIER_NULLABLE)
						,"section.identifier = :"+PARAMETER_NAME_ADMINISTRATIVE_UNIT_SECTION_IDENTIFIER))
				
				/* Processing date */
				,parenthesis(or(String.format(":%s = true", PARAMETER_NAME_PROCESSING_DATE_IS_NULL_NULLABLE),"t.processingDate IS NULL"))
				,parenthesis(or(String.format(":%s = true", PARAMETER_NAME_PROCESSING_DATE_IS_NOT_NULL_NULLABLE),"t.processingDate IS NOT NULL"))
				/* Function */
				,parenthesis(or(String.format(":%s = true", PARAMETER_NAME_FUNCTION_IDENTIFIER_NULLABLE)
						,"EXISTS(SELECT rsf FROM RequestScopeFunction rsf WHERE rsf.request = t AND rsf.scopeFunction.function.identifier = :"
								+PARAMETER_NAME_FUNCTION_IDENTIFIER+")"))
				/* Dispatch slip*/
				,parenthesis(or(String.format(":%s = true", PARAMETER_NAME_DISPATCH_SLIP_IS_NULL_NULLABLE),"rds IS NULL"))
				,parenthesis(or(String.format(":%s = true", PARAMETER_NAME_DISPATCH_SLIP_IS_NOT_NULL_NULLABLE),"rds IS NOT NULL"))
				,parenthesis(or(String.format(":%s = true", PARAMETER_NAME_DISPATCH_SLIP_IDENTIFIER_NULLABLE),"rds.identifier = :"+PARAMETER_NAME_DISPATCH_SLIP_IDENTIFIER))
				
				//,isNullableOrIsNull("rds", Request.FIELD_DISPATCH_SLIP, PARAMETER_NAME_PROCESSING_DATE_IS_NULL_NULLABL)
		));
	}
	
	static String getOrderBy() {
		return order(desc("t",Request.FIELD_CREATION_DATE));
	}
}