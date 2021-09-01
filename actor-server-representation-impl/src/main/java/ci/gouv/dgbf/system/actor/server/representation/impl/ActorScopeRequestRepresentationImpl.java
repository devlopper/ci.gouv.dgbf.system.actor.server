package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;
import java.util.Collection;

import javax.enterprise.context.ApplicationScoped;
import javax.ws.rs.core.Response;

import org.cyk.utility.__kernel__.map.CollectionOfMapsStringStringBuilder;
import org.cyk.utility.__kernel__.rest.RequestProcessor;
import org.cyk.utility.business.TransactionResult;

import ci.gouv.dgbf.system.actor.server.business.api.ActorScopeRequestBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorScopeRequest;
import ci.gouv.dgbf.system.actor.server.representation.api.ActorScopeRequestRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.ActorScopeRequestDto;

@ApplicationScoped
public class ActorScopeRequestRepresentationImpl extends AbstractActorRequestRepresentationImpl<ActorScopeRequestDto,ActorScopeRequest> implements ActorScopeRequestRepresentation,Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	protected TransactionResult __record__(Collection<String> actorsIdentifiers,Collection<String> requestablesIdentifiers, String actorCode, Boolean ignoreExisting) {
		return __inject__(ActorScopeRequestBusiness.class).record(actorsIdentifiers, requestablesIdentifiers, actorCode, ignoreExisting);
	}

	@Override
	protected TransactionResult __cancel__(Collection<String> identifiers, String actorCode, Boolean ignoreExisting) {
		return __inject__(ActorScopeRequestBusiness.class).cancel(identifiers, actorCode, ignoreExisting);
	}

	@Override
	protected TransactionResult __process__(Collection<ActorScopeRequest> requests, String actorCode) {
		return __inject__(ActorScopeRequestBusiness.class).process(requests, actorCode);
	}

	@Override
	protected Class<ActorScopeRequest> getRequestPersistenceClass() {
		return ActorScopeRequest.class;
	}
	
	public static Response getByActorCode(String actorCode,Boolean processed,Boolean granted,Boolean pageable,Integer firstTupleIndex,Integer numberOfTuples) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new Runnable() {
					@Override
					public void run() {
						Collection<ActorScopeRequest> actorScopeRequests = __inject__(ActorScopeRequestBusiness.class).findByActorCode(actorCode, processed,granted, pageable, firstTupleIndex, numberOfTuples);
						CollectionOfMapsStringStringBuilder.Arguments<ActorScopeRequest> arguments = new CollectionOfMapsStringStringBuilder.Arguments<ActorScopeRequest>()
								.setCollection(actorScopeRequests).addFieldsNames(
										ActorScopeRequest.FIELD_IDENTIFIER,ActorScopeRequestDto.JSON_FIELD_IDENTIFIER
										,ActorScopeRequest.FIELD_ACTOR_STRING,ActorScopeRequestDto.JSON_FIELD_ACTOR_AS_STRING
										,ActorScopeRequest.FIELD_SCOPE_AS_STRING,ActorScopeRequestDto.JSON_FIELD_SCOPE_TYPE_AS_STRING
										,ActorScopeRequest.FIELD_SCOPE_TYPE_AS_STRING,ActorScopeRequestDto.JSON_FIELD_SCOPE_AS_STRING
										,ActorScopeRequest.FIELD_COMMENT,ActorScopeRequestDto.JSON_FIELD_COMMENT
										,ActorScopeRequest.FIELD_GRANTED_AS_STRING,ActorScopeRequestDto.JSON_FIELD_GRANTED_AS_STRING
										,ActorScopeRequest.FIELD_PROCESSING_COMMENT,ActorScopeRequestDto.JSON_FIELD_PROCESSING_COMMENT);
						arguments.setEmptyValueAsArrayList();
						responseBuilderArguments.setEntity(CollectionOfMapsStringStringBuilder.getInstance().build(ActorScopeRequest.class, arguments));
					}
				};
			}
		});
	}
	
	/*
	@Override
	public Response record(List<String> actorsIdentifiers, List<String> scopesIdentifiers, String actorCode,Boolean ignoreExisting) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new AbstractRunnableImpl.TransactionImpl(responseBuilderArguments){
					@Override
					public TransactionResult transact() {
						return __inject__(ActorScopeRequestBusiness.class).record(actorsIdentifiers, scopesIdentifiers, actorCode, ignoreExisting);
					}
				};
			}			
		});
	}

	@Override
	public Response cancel(List<String> identifiers, String actorCode,Boolean ignoreExisting) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new AbstractRunnableImpl.TransactionImpl(responseBuilderArguments){
					@Override
					public TransactionResult transact() {
						return __inject__(ActorScopeRequestBusiness.class).cancel(identifiers,actorCode,ignoreExisting);
					}
				};
			}			
		});
	}

	@Override
	public Response process(List<ActorScopeRequestDto> actorScopeRequestDtos,String actorCode) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new AbstractRunnableImpl.TransactionImpl(responseBuilderArguments){
					@Override
					public TransactionResult transact() {
						ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("actorScopeRequestDtos", actorScopeRequestDtos);
						Collection<ActorScopeRequest> actorScopeRequests = EntityFinder.getInstance().findMany(ActorScopeRequest.class, FieldHelper.readSystemIdentifiersAsStrings(actorScopeRequestDtos));
						ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("actorScopeRequests", actorScopeRequests);
						actorScopeRequests.forEach(actorScopeRequest -> {
							for(ActorScopeRequestDto dto : actorScopeRequestDtos)
								if(actorScopeRequest.getIdentifier().equals(dto.getIdentifier())) {
									actorScopeRequest.setGranted(dto.getGranted());
									actorScopeRequest.setProcessingComment(dto.getProcessingComment());
									break;
								}
						});
						return __inject__(ActorScopeRequestBusiness.class).process(actorScopeRequests, actorCode);
					}
				};
			}			
		});
	}
	*/
}