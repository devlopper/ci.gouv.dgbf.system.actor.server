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
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.stream.Collectors;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.computation.ArithmeticOperator;
import org.cyk.utility.__kernel__.constant.ConstantEmpty;
import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.__kernel__.instance.InstanceCopier;
import org.cyk.utility.__kernel__.map.MapHelper;
import org.cyk.utility.persistence.EntityManagerGetter;
import org.cyk.utility.persistence.server.procedure.ProcedureExecutor;
import org.cyk.utility.persistence.query.EntityFinder;
import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutor;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.query.QueryHelper;
import org.cyk.utility.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.persistence.query.QueryName;
import org.cyk.utility.persistence.query.Filter;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.time.TimeHelper;
import org.cyk.utility.__kernel__.value.Value;
import org.cyk.utility.persistence.server.TransientFieldsProcessor;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.IdentificationAttribute;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Identity;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Request;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestStatus;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestType;

public interface RequestQuerier extends Querier {

	Integer NUMBER_OF_WORDS = 6;
	
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
	
	String PARAMETER_NAME_ADMINISTRATIVE_UNIT = "administrativeunit";
	
	String PARAMETER_NAME_ADMINISTRATIVE_UNIT_IDENTIFIER = "administrativeunitIdentifier";
	String PARAMETER_NAME_ADMINISTRATIVE_UNIT_SECTION_IDENTIFIER = PARAMETER_NAME_ADMINISTRATIVE_UNIT+"SectionIdentifier";
	String PARAMETER_NAME_ADMINISTRATIVE_UNIT_SECTION_IDENTIFIER_NULLABLE = PARAMETER_NAME_ADMINISTRATIVE_UNIT_SECTION_IDENTIFIER+"Nullable";
	
	String PARAMETER_NAME_FUNCTION_IDENTIFIER = "functionIdentifier";
	String PARAMETER_NAME_FUNCTION_IDENTIFIER_NULLABLE = PARAMETER_NAME_FUNCTION_IDENTIFIER+"Nullable";
	
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
	
	String QUERY_IDENTIFIER_INSTANTIATE_ONE_BY_TYPE_IDENTIFIER_BY_ACTOR_IDENTIFIER = QueryIdentifierBuilder.getInstance().build(Request.class, "instantiateOneByTypeIdentifierByActorIdentifier");
	Request instantiateOneByTypeIdentifierByActorIdentifier(String typeIdentifier,String actorIdentifier);
	
	String QUERY_IDENTIFIER_READ_BY_IDENTIFIER = QueryIdentifierBuilder.getInstance().build(Request.class, "readByIdentifier");
	Request readByIdentifier(String identifier);
	
	String QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_UI = QueryIdentifierBuilder.getInstance().build(Request.class, "readByIdentifierForUI");
	Request readByIdentifierForUI(String identifier);
	
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
	
	void exportForAccountCreation(String actor,String functionality,String action,Date date);
	
	public static abstract class AbstractImpl extends Querier.AbstractImpl implements RequestQuerier,Serializable {
		
		@Override
		public Request readOne(QueryExecutorArguments arguments) {
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_INSTANTIATE_ONE_BY_TYPE_IDENTIFIER))
				return instantiateOneByTypeIdentifier((String)arguments.getFilterFieldValue(PARAMETER_NAME_TYPE_IDENTIFIER));
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_INSTANTIATE_ONE_BY_TYPE_IDENTIFIER_BY_ACTOR_IDENTIFIER))
				return instantiateOneByTypeIdentifierByActorIdentifier((String)arguments.getFilterFieldValue(PARAMETER_NAME_TYPE_IDENTIFIER)
						,(String)arguments.getFilterFieldValue(PARAMETER_NAME_ACTOR_IDENTIFIER));
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_BY_IDENTIFIER))
				return readByIdentifier((String)arguments.getFilterFieldValue(PARAMETER_NAME_IDENTIFIER));
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_UI))
				return readByIdentifierForUI((String)arguments.getFilterFieldValue(PARAMETER_NAME_IDENTIFIER));
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_EDIT))
				return readByIdentifierForEdit((String)arguments.getFilterFieldValue(PARAMETER_NAME_IDENTIFIER));
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_BY_ACCESS_TOKEN))
				return readByAccessToken((String)arguments.getFilterFieldValue(PARAMETER_NAME_ACCESS_TOKEN));
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_BY_ACCESS_TOKEN_FOR_UI))
				return readByAccessTokenForUI((String)arguments.getFilterFieldValue(PARAMETER_NAME_ACCESS_TOKEN));
			throw new RuntimeException(arguments.getQuery().getIdentifier()+" cannot be processed");
		}
		
		@Override
		public Collection<Request> readMany(QueryExecutorArguments arguments) {
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_WHERE_FILTER))
				return readWhereFilter(arguments);
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_UI))
				return readWhereFilterForUI(arguments);
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_BY_DISPATCH_SLIP_IDENTIFIER))
				return readByDispatchSlipIdentifier((String) arguments.getFilterFieldValue(PARAMETER_NAME_REQUEST_DISPATCH_SLIP_IDENTIFIER));
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_BY_DISPATCH_SLIP_IDENTIFIER_FOR_UI))
				return readByDispatchSlipIdentifierForUI((String) arguments.getFilterFieldValue(PARAMETER_NAME_REQUEST_DISPATCH_SLIP_IDENTIFIER));
			throw new RuntimeException(arguments.getQuery().getIdentifier()+" cannot be processed");
		}
		
		@Override
		public Long count(QueryExecutorArguments arguments) {
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_COUNT_WHERE_FILTER))
				return countWhereFilter(arguments);
			throw new RuntimeException(arguments.getQuery().getIdentifier()+" cannot be processed");
		}
		
		@Override
		public Request instantiateOneByTypeIdentifier(String typeIdentifier) {
			if(StringHelper.isBlank(typeIdentifier))
				return null;
			RequestType type = EntityFinder.getInstance().find(RequestType.class, typeIdentifier);
			if(type == null)
				return null;
			Request request = new Request().setType(type);
			IdentificationFormQuerier.AbstractImpl.setFields(type.getForm(), null);
			request.setAuthenticationRequired(request.getType().getAuthenticationRequired());
			return request;
		}
		
		@Override
		public Request instantiateOneByTypeIdentifierByActorIdentifier(String typeIdentifier,String actorIdentifier) {
			if(StringHelper.isBlank(typeIdentifier) || StringHelper.isBlank(actorIdentifier))
				return null;
			Request request = instantiateOneByTypeIdentifier(typeIdentifier);
			if(request == null)
				return null;
			//Actor actor = ActorQuerier.getInstance().readProfileInformationsByIdentifierForUI(actorIdentifier);
			Actor actor = EntityFinder.getInstance().find(Actor.class, actorIdentifier);
			if(actor != null) {
				request.setActor(new Actor().setIdentifier(actor.getIdentifier()));
				Map<String,IdentificationAttribute> fieldsNamesMap = Request.computeFieldsNames(request.getType().getForm());
				if(MapHelper.isNotEmpty(fieldsNamesMap)) {
					Collection<String> fieldsNames = null;
					for(Map.Entry<String, IdentificationAttribute> entry : fieldsNamesMap.entrySet()) {
						if(FieldHelper.getByName(Identity.class, entry.getKey()) == null)
							continue;
						if(fieldsNames == null)
							fieldsNames = new ArrayList<>();
						fieldsNames.add(entry.getKey());
					}
					if(CollectionHelper.isNotEmpty(fieldsNames)) {
						InstanceCopier.getInstance().copy(actor.getIdentity(), request, fieldsNames);
						if(fieldsNames.contains(Request.FIELD_ADMINISTRATIVE_UNIT))
							request.setAdministrativeUnit(actor.getIdentity().getAdministrativeUnit());
						if(fieldsNames.contains(Request.FIELD_SECTION) && actor.getIdentity().getAdministrativeUnit() != null)
							request.setSection(actor.getIdentity().getAdministrativeUnit().getSection());	
						if(fieldsNames.contains(Request.FIELD_ACT_OF_APPOINTMENT_SIGNATURE_DATE) && actor.getIdentity().getActOfAppointmentSignatureDate() != null)
							request.setActOfAppointmentSignatureDateAsTimestamp(TimeHelper.toMillisecond(actor.getIdentity().getActOfAppointmentSignatureDate()));
					}					
				}			
			}
			return request;
		}
		
		@Override
		public Request readByIdentifier(String identifier) {
			return QueryExecutor.getInstance().executeReadOne(Request.class, new QueryExecutorArguments().setQueryFromIdentifier(QUERY_IDENTIFIER_READ_BY_IDENTIFIER)
					.addFilterField(PARAMETER_NAME_IDENTIFIER, identifier));
		}
		
		@Override
		public Request readByIdentifierForUI(String identifier) {
			Request request = readByIdentifier(identifier);
			if(request == null)
				return null;
			TransientFieldsProcessor.getInstance().process(List.of(request), List.of(Request.FIELD_GRANTED_BUDGETARIES_SCOPE_FUNCTIONS
					,Request.FIELD_BUDGETARIES_SCOPE_FUNCTIONS_AS_STRINGS,Request.FIELD_BUDGETARIES_SCOPE_FUNCTIONS_GRANTED_AS_STRINGS));
			prepareForUI(request);			
			return request;
		}
		
		private void prepareForUI(Request request) {
			if(request == null)
				return;
			if(request.getCreationDate() != null) {
				request.setCreationDateAsString(TimeHelper.formatLocalDateTime(request.getCreationDate(),"dd/MM/yyyy à HH:mm"));
				request.setCreationDate(null);
			}
			
			if(request.getProcessingDate() != null) {
				request.setProcessingDateAsString(TimeHelper.formatLocalDateTime(request.getProcessingDate(),"dd/MM/yyyy à HH:mm"));
				request.setProcessingDate(null);
			}
			
			if(request.getActOfAppointment() != null)
				request.setActOfAppointmentIdentifier(request.getIdentifier());
			if(request.getPhoto() != null)
				request.setPhotoIdentifier(request.getIdentifier());
			if(request.getSignature() != null)
				request.setSignatureIdentifier(request.getIdentifier());
			if(request.getSignedRequestSheet() != null)
				request.setSignedRequestSheetIdentifier(request.getIdentifier());
			
			if(request.getStatus() != null) {
				if(StringHelper.isBlank(request.getStatusAsString()))
					request.setStatusAsString(request.getStatus().getName());
				request.setAccepted(RequestStatus.CODE_ACCEPTED.equals(request.getStatus().getCode()));
				request.setRejected(RequestStatus.CODE_REJECTED.equals(request.getStatus().getCode()));
				request.setProcessed(Boolean.TRUE.equals(request.getAccepted()) || Boolean.TRUE.equals(request.getRejected()));
			}
			
			if(request.getActor() != null)
				request.setActorCode(request.getActor().getCode());
			request.setActorNames(Identity.getNames((String)FieldHelper.readName(request.getCivility()), request.getFirstName(), request.getLastNames()));
			
			if(request.getType() != null) {
				request.setTypeAsString(request.getType().getName());
				IdentificationFormQuerier.AbstractImpl.setFields(request.getType().getForm(), null);
				
				if(RequestType.CODE_DEMANDE_POSTES_BUDGETAIRES.equals(request.getType().getCode())) {
					if(request.getAdministrativeUnit() != null && request.getAdministrativeUnit().getSection() != null && StringHelper.isBlank(request.getSectionAsString()))
						request.setSectionAsString(request.getAdministrativeUnit().getSection().toString());
				}
			}
			
			if(request.getActOfAppointmentSignatureDate() != null) {
				request.setActOfAppointmentSignatureDateAsTimestamp(request.getActOfAppointmentSignatureDate().atStartOfDay().toInstant(ZoneOffset.UTC).toEpochMilli());
				request.setActOfAppointmentSignatureDateAsString(DateTimeFormatter.ofPattern("dd/MM/yyyy", Locale.FRENCH)
						.format(request.getActOfAppointmentSignatureDate()));
				request.setActOfAppointmentSignatureDate(null);
			}
			
			request.setFirstNameAndLastNames(request.getFirstName()+" "+request.getLastNames());
		}
		
		@Override
		public Request readByIdentifierForEdit(String identifier) {
			Request request = readByIdentifier(identifier);
			if(request == null)
				return null;
			if(request.getActOfAppointmentSignatureDate() != null) {
				request.setActOfAppointmentSignatureDateAsTimestamp(request.getActOfAppointmentSignatureDate().atStartOfDay().toInstant(ZoneOffset.UTC).toEpochMilli());
				request.setActOfAppointmentSignatureDateAsString(DateTimeFormatter.ofPattern("dd/MM/yyyy", Locale.FRENCH)
						.format(request.getActOfAppointmentSignatureDate()));
				request.setActOfAppointmentSignatureDate(null);
			}
			IdentificationFormQuerier.AbstractImpl.setFields(request.getType().getForm(), null);
			Collection<RequestScopeFunction> requestScopeFunctions = RequestScopeFunctionQuerier.getInstance().readByRequestsIdentifiers(List.of(request.getIdentifier()));
			if(CollectionHelper.isNotEmpty(requestScopeFunctions)) {
				request.setBudgetariesScopeFunctions(requestScopeFunctions.stream()
					.filter(x -> x.getRequest().getIdentifier().equals(request.getIdentifier()) && Boolean.TRUE.equals(x.getRequested()))
					.map(x -> x.getScopeFunction())
					.collect(Collectors.toList()));				
			}
			if(request.getType() != null && RequestType.IDENTIFIER_DEMANDE_POSTES_BUDGETAIRES.equals(request.getType().getIdentifier())) {
				request.setSection(request.getAdministrativeUnit().getSection());
			}
			return request;
		}
		
		@Override
		public Request readByAccessToken(String accessToken) {
			return QueryExecutor.getInstance().executeReadOne(Request.class, new QueryExecutorArguments().setQueryFromIdentifier(QUERY_IDENTIFIER_READ_BY_ACCESS_TOKEN)
					.addFilterField(PARAMETER_NAME_ACCESS_TOKEN, accessToken));
		}
		
		@Override
		public Request readByAccessTokenForUI(String accessToken) {
			Request request = readByAccessToken(accessToken);
			if(request == null)
				return null;
			prepareForUI(request);
			return request;
		}
		
		@Override
		public Collection<Request> readWhereFilter(QueryExecutorArguments arguments) {
			if(arguments == null)
				arguments = new QueryExecutorArguments();
			if(arguments.getQuery() == null)
				arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_WHERE_FILTER);
			prepareWhereFilter(arguments);
			return QueryExecutor.getInstance().executeReadMany(Request.class, arguments);
		}
		
		@Override
		public Collection<Request> readWhereFilterForUI(QueryExecutorArguments arguments) {
			if(arguments == null)
				arguments = new QueryExecutorArguments();
			arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_WHERE_FILTER);
			Collection<Request> requests = readWhereFilter(arguments);
			if(CollectionHelper.isEmpty(requests))
				return null;
			TransientFieldsProcessor.getInstance().process(requests, arguments.getProcessableTransientFieldsNames());
			requests.forEach(request -> {
				prepareForUI(request);
			});
			return requests;
		}
		
		@Override
		public Long countWhereFilter(QueryExecutorArguments arguments) {
			if(arguments == null)
				arguments = new QueryExecutorArguments();
			if(arguments.getQuery() == null)
				arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_COUNT_WHERE_FILTER);
			prepareWhereFilter(arguments);
			return QueryExecutor.getInstance().executeCount(arguments);
		}
		
		private static void prepareWhereFilter(QueryExecutorArguments arguments) {
			Filter filter = new Filter();
			filter.addFieldsNullable(arguments
					, PARAMETER_NAME_ACTOR_IDENTIFIER
					,PARAMETER_NAME_STATUS_IDENTIFIER
					,PARAMETER_NAME_REGISTRATION_NUMBER
					,PARAMETER_NAME_ADMINISTRATIVE_UNIT_SECTION_IDENTIFIER
					,PARAMETER_NAME_FUNCTION_IDENTIFIER
					,PARAMETER_NAME_DISPATCH_SLIP_IDENTIFIER
					,PARAMETER_NAME_PROCESSING_DATE_IS_NULL,PARAMETER_NAME_PROCESSING_DATE_IS_NOT_NULL
					,PARAMETER_NAME_DISPATCH_SLIP_IS_NULL,PARAMETER_NAME_DISPATCH_SLIP_IS_NOT_NULL
					);
			
			filter.addFieldEquals(PARAMETER_NAME_ACTOR_IDENTIFIER, arguments);
			filter.addFieldEquals(PARAMETER_NAME_STATUS_IDENTIFIER, arguments);
			filter.addFieldEquals(PARAMETER_NAME_ADMINISTRATIVE_UNIT_SECTION_IDENTIFIER, arguments);
			filter.addFieldEquals(PARAMETER_NAME_FUNCTION_IDENTIFIER, arguments);
			filter.addFieldEquals(PARAMETER_NAME_DISPATCH_SLIP_IDENTIFIER, arguments);
			
			filter.addFieldContains(PARAMETER_NAME_CODE, arguments);
			filter.addFieldContains(PARAMETER_NAME_ELECTRONIC_MAIL_ADDRESS, arguments);
			filter.addFieldContains(PARAMETER_NAME_FIRST_NAME, arguments);
			filter.addFieldContains(PARAMETER_NAME_REGISTRATION_NUMBER, arguments);
			filter.addFieldContainsStringOrWords(PARAMETER_NAME_LAST_NAMES, NUMBER_OF_WORDS, arguments);			
			//filter.addFieldContainsStringOrWords(PARAMETER_NAME_ADMINISTRATIVE_UNIT, NUMBER_OF_WORDS, arguments);
			
			@SuppressWarnings("unchecked")
			Collection<String> excludedIdentifiers = (Collection<String>) arguments.getFilterFieldValue(PARAMETER_NAME_EXCLUDED_IDENTIFIERS);
			if(CollectionHelper.isEmpty(excludedIdentifiers))
				excludedIdentifiers = ConstantEmpty.STRINGS_WITH_ONE_ELEMENT;
			filter.addField(PARAMETER_NAME_EXCLUDED_IDENTIFIERS, excludedIdentifiers,null,ArithmeticOperator.IN);
			
			arguments.setFilter(filter);
		}
		
		@Override
		public Collection<Request> readByElectronicMailAddress(String electronicMailAddress) {
			return QueryExecutor.getInstance().executeReadMany(Request.class, QUERY_IDENTIFIER_READ_BY_ELECTRONIC_MAIL_ADDRESS,PARAMETER_NAME_ELECTRONIC_MAIL_ADDRESS
					,electronicMailAddress);
		}
		
		@Override
		public Collection<Request> readByDispatchSlipIdentifier(String dispatchSlipIdentifier) {
			return QueryExecutor.getInstance().executeReadMany(Request.class, QUERY_IDENTIFIER_READ_BY_DISPATCH_SLIP_IDENTIFIER,PARAMETER_NAME_REQUEST_DISPATCH_SLIP_IDENTIFIER
					,dispatchSlipIdentifier);
		}
		
		@Override
		public Collection<Request> readByDispatchSlipIdentifierForUI(String dispatchSlipIdentifier) {
			Collection<Request> requests = QueryExecutor.getInstance().executeReadMany(Request.class, QUERY_IDENTIFIER_READ_BY_DISPATCH_SLIP_IDENTIFIER,PARAMETER_NAME_REQUEST_DISPATCH_SLIP_IDENTIFIER
					,dispatchSlipIdentifier);
			if(CollectionHelper.isEmpty(requests))
				return null;
			TransientFieldsProcessor.getInstance().process(requests, List.of(Request.FIELD_BUDGETARIES_SCOPE_FUNCTIONS_AS_STRINGS
					,Request.FIELD_BUDGETARIES_SCOPE_FUNCTIONS_GRANTED_AS_STRINGS));
			for(Request request : requests)
				prepareForUI(request);			
			return requests;
		}
		
		@Override
		public byte[] readPhotoByIdentifier(String identifier) {
			return EntityManagerGetter.getInstance().get().createNamedQuery(QUERY_IDENTIFIER_READ_PHOTO_BY_IDENTIFIER
					, byte[].class).setParameter(PARAMETER_NAME_IDENTIFIER,identifier).getSingleResult();
		}
		
		@Override
		public byte[] readActOfAppointmentByIdentifier(String identifier) {
			return EntityManagerGetter.getInstance().get().createNamedQuery(QUERY_IDENTIFIER_READ_ACT_OF_APPOINTMENT_BY_IDENTIFIER
					, byte[].class).setParameter(PARAMETER_NAME_IDENTIFIER,identifier).getSingleResult();
		}
		
		@Override
		public byte[] readSignatureByIdentifier(String identifier) {
			return EntityManagerGetter.getInstance().get().createNamedQuery(QUERY_IDENTIFIER_READ_SIGNATURE_BY_IDENTIFIER
					, byte[].class).setParameter(PARAMETER_NAME_IDENTIFIER,identifier).getSingleResult();
		}
		
		@Override
		public byte[] readSignedRequestSheetByIdentifier(String identifier) {
			return EntityManagerGetter.getInstance().get().createNamedQuery(QUERY_IDENTIFIER_READ_SIGNED_REQUEST_SHEET_BY_IDENTIFIER
					, byte[].class).setParameter(PARAMETER_NAME_IDENTIFIER,identifier).getSingleResult();
		}
		
		@Override
		public void exportForAccountCreation(String actor, String functionality, String action, Date date) {
			ProcedureExecutor.getInstance().execute(Request.STORED_PROCEDURE_QUERY_PROCEDURE_NAME_CREATE_USERS);
		}
		
		/*protected static void setFunctions(Collection<Request> requests,Boolean asString) {
			Collection<RequestFunction> requestFunctions = RequestFunctionQuerier.getInstance().readByRequestsIdentifiers(FieldHelper.readSystemIdentifiersAsStrings(requests));
			if(CollectionHelper.isEmpty(requestFunctions))
				return;
			requests.forEach(request -> {
				Collection<Function> functions = requestFunctions.stream().filter(x -> x.getRequest().equals(request)).map(x -> x.getFunction()).collect(Collectors.toList());
				if(CollectionHelper.isNotEmpty(functions)) {
					if(Boolean.TRUE.equals(asString))
						request.setFunctionsAsStrings(functions.stream().map(function -> function.getName()).collect(Collectors.toList()));
					else
						request.setFunctions(requestFunctions.stream().filter(x -> x.getRequest().equals(request)).map(x -> x.getFunction()).collect(Collectors.toList()));
				}
				
			});
		}*/
	}
	
	/**/
	
	/**/
	
	static RequestQuerier getInstance() {
		return Helper.getInstance(RequestQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		QueryHelper.addQueries(
			Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_WHERE_FILTER
			,Query.FIELD_TUPLE_CLASS,Request.class,Query.FIELD_RESULT_CLASS,Request.class
			,Query.FIELD_VALUE,jpql(select(
					fields("t",Request.FIELD_IDENTIFIER,Request.FIELD_CODE,Request.FIELD_FIRST_NAME,Request.FIELD_LAST_NAMES,Request.FIELD_REGISTRATION_NUMBER
							,Request.FIELD_ELECTRONIC_MAIL_ADDRESS,Request.FIELD_MOBILE_PHONE_NUMBER,Request.FIELD_STATUS
							,Request.FIELD_CREATION_DATE,Request.FIELD_PROCESSING_DATE)
					,fields("a",Actor.FIELD_CODE),concat("t", Actor.FIELD_FIRST_NAME,Actor.FIELD_LAST_NAMES) 
					,fields("rt",RequestType.FIELD_NAME),fields("rs",RequestStatus.FIELD_NAME),concatCodeName("au"),concatCodeName("section")
					)
					,getReadWhereFilterFromWhere(),getOrderBy())
			).setTupleFieldsNamesIndexesFromFieldsNames(Request.FIELD_IDENTIFIER,Request.FIELD_CODE,Request.FIELD_FIRST_NAME,Request.FIELD_LAST_NAMES
					,Request.FIELD_REGISTRATION_NUMBER,Request.FIELD_ELECTRONIC_MAIL_ADDRESS,Request.FIELD_MOBILE_PHONE_NUMBER,Request.FIELD_STATUS
					,Request.FIELD_CREATION_DATE_AS_STRING,Request.FIELD_PROCESSING_DATE_AS_STRING
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
		);
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