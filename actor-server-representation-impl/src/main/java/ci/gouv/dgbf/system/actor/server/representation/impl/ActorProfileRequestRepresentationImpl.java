package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;
import java.util.Collection;

import javax.enterprise.context.ApplicationScoped;
import javax.ws.rs.core.Response;

import org.cyk.utility.__kernel__.map.CollectionOfMapsStringStringBuilder;
import org.cyk.utility.__kernel__.rest.RequestProcessor;
import org.cyk.utility.business.TransactionResult;

import ci.gouv.dgbf.system.actor.server.business.api.ActorProfileRequestBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorProfileRequest;
import ci.gouv.dgbf.system.actor.server.representation.api.ActorProfileRequestRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.ActorProfileRequestDto;

@ApplicationScoped
public class ActorProfileRequestRepresentationImpl extends AbstractActorRequestRepresentationImpl<ActorProfileRequestDto,ActorProfileRequest> implements ActorProfileRequestRepresentation,Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	protected TransactionResult __record__(Collection<String> actorsIdentifiers,Collection<String> requestablesIdentifiers, String actorCode, Boolean ignoreExisting) {
		return __inject__(ActorProfileRequestBusiness.class).record(actorsIdentifiers, requestablesIdentifiers, actorCode, ignoreExisting);
	}

	@Override
	protected TransactionResult __cancel__(Collection<String> identifiers, String actorCode, Boolean ignoreExisting) {
		return __inject__(ActorProfileRequestBusiness.class).cancel(identifiers, actorCode, ignoreExisting);
	}

	@Override
	protected TransactionResult __process__(Collection<ActorProfileRequest> requests, String actorCode) {
		return __inject__(ActorProfileRequestBusiness.class).process(requests, actorCode);
	}

	@Override
	protected Class<ActorProfileRequest> getRequestPersistenceClass() {
		return ActorProfileRequest.class;
	}
	
	public static Response getByActorCode(String actorCode,Boolean processed,Boolean granted,Boolean pageable,Integer firstTupleIndex,Integer numberOfTuples) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new Runnable() {
					@Override
					public void run() {
						Collection<ActorProfileRequest> actorProfileRequests = __inject__(ActorProfileRequestBusiness.class).findByActorCode(actorCode, processed,granted, pageable, firstTupleIndex, numberOfTuples);
						CollectionOfMapsStringStringBuilder.Arguments<ActorProfileRequest> arguments = new CollectionOfMapsStringStringBuilder.Arguments<ActorProfileRequest>()
								.setCollection(actorProfileRequests).addFieldsNames(
										ActorProfileRequest.FIELD_IDENTIFIER,ActorProfileRequestDto.JSON_FIELD_IDENTIFIER
										,ActorProfileRequest.FIELD_ACTOR_STRING,ActorProfileRequestDto.JSON_FIELD_ACTOR_AS_STRING
										,ActorProfileRequest.FIELD_PROFILE_AS_STRING,ActorProfileRequestDto.JSON_FIELD_PROFILE_AS_STRING
										,ActorProfileRequest.FIELD_PROFILE_TYPE_AS_STRING,ActorProfileRequestDto.JSON_FIELD_PROFILE_TYPE_AS_STRING
										,ActorProfileRequest.FIELD_COMMENT,ActorProfileRequestDto.JSON_FIELD_COMMENT
										,ActorProfileRequest.FIELD_GRANTED_AS_STRING,ActorProfileRequestDto.JSON_FIELD_GRANTED_AS_STRING
										,ActorProfileRequest.FIELD_PROCESSING_COMMENT,ActorProfileRequestDto.JSON_FIELD_PROCESSING_COMMENT);
						arguments.setEmptyValueAsArrayList();
						responseBuilderArguments.setEntity(CollectionOfMapsStringStringBuilder.getInstance().build(ActorProfileRequest.class, arguments));
					}
				};
			}
		});
	}
	
	/**/
}