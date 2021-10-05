package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.stream.Collectors;

import javax.persistence.EntityManager;

import org.cyk.utility.__kernel__.array.ArrayHelper;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.computation.ArithmeticOperator;
import org.cyk.utility.__kernel__.computation.SortOrder;
import org.cyk.utility.__kernel__.constant.ConstantEmpty;
import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.__kernel__.instance.InstanceCopier;
import org.cyk.utility.__kernel__.map.MapHelper;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.time.TimeHelper;
import org.cyk.utility.persistence.EntityManagerGetter;
import org.cyk.utility.persistence.query.EntityFinder;
import org.cyk.utility.persistence.query.Filter;
import org.cyk.utility.persistence.query.QueryExecutor;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.server.TransientFieldsProcessor;
import org.cyk.utility.persistence.server.procedure.ProcedureExecutor;
import org.cyk.utility.persistence.server.procedure.ProcedureExecutorArguments;
import org.cyk.utility.persistence.server.query.ReaderByCollection;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.IdentificationFormQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.RequestQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.RequestScopeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.IdentificationAttribute;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Identity;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Request;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestStatus;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestType;

public class RequestQuerierImpl extends RequestQuerier.AbstractImpl {
	
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
			return readByIdentifierForUI(arguments);
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
	public Request instantiateOneByTypeIdentifierByElectronicMailAddress(String typeIdentifier,String electronicMailAddress) {
		if(StringHelper.isBlank(typeIdentifier) || StringHelper.isBlank(electronicMailAddress))
			return null;
		RequestType type = EntityFinder.getInstance().find(RequestType.class, typeIdentifier);
		if(type == null)
			return null;
		Collection<Request> requests = EntityManagerGetter.getInstance().get().createQuery("SELECT t FROM Request t "
			+ "WHERE t.type.identifier = :typeIdentifier AND t.electronicMailAddress = :electronicMailAddress ORDER BY t.creationDate DESC", Request.class)
				.setParameter("typeIdentifier", typeIdentifier).setParameter("electronicMailAddress", electronicMailAddress)
			.setMaxResults(1).getResultList();
		if(CollectionHelper.getSize(requests) > 1)
			throw new RuntimeException("Trop de demandes récentes trouvées");
		Request request = CollectionHelper.getFirst(requests);
		if(request == null)
			request = new Request();
		request.setType(type);
		IdentificationFormQuerier.AbstractImpl.setFields(type.getForm(), null);
		if(request.getActOfAppointmentSignatureDate() != null)
			request.setActOfAppointmentSignatureDateAsTimestamp(TimeHelper.toMillisecond(request.getActOfAppointmentSignatureDate()));
		request.setAuthenticationRequired(request.getType().getAuthenticationRequired());
		request.setIdentifier(null);
		request.setCode(null);
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
	public Request readByIdentifierForUI(QueryExecutorArguments arguments) {
		if(arguments != null /*&& arguments.getQuery() == null*/)
			arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_BY_IDENTIFIER);
		Request request = QueryExecutor.getInstance().executeReadOne(Request.class, arguments);
		if(request == null)
			return null;
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
		
		if(MapHelper.isNotEmpty(arguments.getSortOrders())) {
			Map<String,SortOrder> orders = new LinkedHashMap<>();
			for(Map.Entry<String,SortOrder> entry : arguments.getSortOrders().entrySet()) {
				orders.put("t."+entry.getKey(), entry.getValue());
			}
			arguments.setSortOrders(orders);
		}
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
		TransientFieldsProcessor.getInstance().process(requests,null, List.of(Request.FIELD_BUDGETARIES_SCOPE_FUNCTIONS_AS_STRINGS
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
	public void exportForAccountCreation(String actor, String functionality, String action, Date date,EntityManager entityManager) {
		ProcedureExecutorArguments arguments = new ProcedureExecutorArguments();
		arguments.setName(Request.STORED_PROCEDURE_QUERY_PROCEDURE_NAME_CREATE_USERS);
		arguments.setEntityManager(entityManager);
		ProcedureExecutor.getInstance().execute(arguments);
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public Collection<Object[]> readFromViewByElectronicMailAddresses(Collection<String> emails) {
		if(CollectionHelper.isEmpty(emails))
			return null;
		return new ReaderByCollection.AbstractImpl<String, Object[]>(){

			@Override
			protected Collection<Object[]> __read__(Collection<String> values) {
				return EntityManagerGetter.getInstance().get().createNativeQuery("SELECT email,message FROM V_APP_EX_COMPTE_ERREUR WHERE email IN :emails")
						.setParameter("emails", values).getResultList();
			}				
		}.read(emails);
		
		/*
		Collection<Object[]> collection = null;
		for(List<String> list : CollectionHelper.getBatches((List<String>) emails, 1000)) {
			Collection<Object[]> result = EntityManagerGetter.getInstance().get().createNativeQuery("SELECT email,message FROM V_APP_EX_COMPTE_ERREUR WHERE email IN :emails")
					.setParameter("emails", list).getResultList();
			if(CollectionHelper.isNotEmpty(result)) {
				if(collection == null)
					collection = new ArrayList<>();
				collection.addAll(result);
			}
		}
		return collection;
		*/
	}
	
	@Override
	public Collection<Object[]> readFromViewByElectronicMailAddresses(String... emails) {
		if(ArrayHelper.isEmpty(emails))
			return null;
		return readFromViewByElectronicMailAddresses(CollectionHelper.listOf(emails));
	}
	
	@Override
	public Collection<Object[]> readFromViewByRequests(Collection<Request> requests) {
		if(CollectionHelper.isEmpty(requests))
			return null;
		return readFromViewByElectronicMailAddresses(requests.stream().filter(x -> StringHelper.isNotBlank(x.getElectronicMailAddress()))
				.map(x -> x.getElectronicMailAddress()).collect(Collectors.toList()));
	}
	
	@Override
	public Collection<Object[]> readFromViewByRequests(Request... requests) {
		if(ArrayHelper.isEmpty(requests))
			return null;
		return readFromViewByRequests(CollectionHelper.listOf(requests));
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