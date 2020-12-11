package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;

import javax.enterprise.context.ApplicationScoped;
import javax.transaction.Transactional;

import org.cyk.utility.__kernel__.business.EntitySaver;
import org.cyk.utility.__kernel__.business.EntitySaver.Arguments;
import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.__kernel__.map.MapHelper;
import org.cyk.utility.__kernel__.object.marker.IdentifiableSystem;
import org.cyk.utility.__kernel__.persistence.query.EntityFinder;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.random.RandomHelper;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.throwable.ThrowablesMessages;
import org.cyk.utility.__kernel__.value.ValueHelper;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

import ci.gouv.dgbf.system.actor.server.business.api.RequestBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.RequestPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.IdentificationFormQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.IdentificationAttribute;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Request;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestStatus;

@ApplicationScoped
public class RequestBusinessImpl extends AbstractBusinessEntityImpl<Request, RequestPersistence> implements RequestBusiness,Serializable {
	private static final long serialVersionUID = 1L;
	
	@Override @Transactional
	public void initialize(Request request) {
		if(request == null)
			throw new RuntimeException("La demande est obligatoire");
		if(request.getType() == null)
			throw new RuntimeException("Le type de demande est obligatoire");
		if(request.getType().getForm() == null)
			throw new RuntimeException("Le formulaire de demande est obligatoire");
		if(Boolean.TRUE.equals(request.getAuthenticationRequired())) {
			if(request.getActor() == null)
				throw new RuntimeException("L'acteur qui demande est obligatoire");
		}else {
			if(StringHelper.isBlank(request.getElectronicMailAddress()))
				throw new RuntimeException("Le mail du demandeur est obligatoire");
		}
		request.setStatus(EntityFinder.getInstance().find(RequestStatus.class, RequestStatus.CODE_INITIALIZED));
		IdentificationFormQuerier.AbstractImpl.setFields(request.getType().getForm(), null);
		Map<String,IdentificationAttribute> attributs = Request.computeFieldsNames(request.getType().getForm());
		if(MapHelper.isNotEmpty(attributs)) {
			ThrowablesMessages throwablesMessages = new ThrowablesMessages();
			attributs.forEach( (fieldName,attribut) -> {
				Object fieldValue = FieldHelper.read(request, fieldName);
				if(Boolean.TRUE.equals(attribut.getRequired()))
					if(ValueHelper.isBlank(fieldValue))
						throwablesMessages.add(attribut.getName()+" est obligatoire");
			});
			throwablesMessages.throwIfNotEmpty();
		}
		if(StringHelper.isBlank(request.getIdentifier())) {
			request.setIdentifier("DM_"+IdentifiableSystem.generateRandomly());
			request.setCreationDate(LocalDateTime.now());
			if(request.getActor() == null) {
				request.setAccessToken(RandomHelper.getAlphanumeric(12));
			}
		}
		EntitySaver.getInstance().save(Request.class, new Arguments<Request>()
				.setPersistenceArguments(new org.cyk.utility.__kernel__.persistence.EntitySaver.Arguments<Request>().setCreatables(List.of(request))));
	}
	
	@Override @Transactional
	public void accept(Request request) {
		if(request == null)
			throw new RuntimeException("La demande est obligatoire");
		request.setStatus(EntityFinder.getInstance().find(RequestStatus.class, RequestStatus.CODE_ACCEPTED));
		request.setProcessingDate(LocalDateTime.now());
		EntitySaver.getInstance().save(Request.class, new Arguments<Request>()
				.setPersistenceArguments(new org.cyk.utility.__kernel__.persistence.EntitySaver.Arguments<Request>().setUpdatables(List.of(request))));
	}
	
	@Override @Transactional
	public void acceptByIdentifier(String identifier) {
		Request request = EntityFinder.getInstance().find(Request.class, new QueryExecutorArguments().addSystemIdentifiers(identifier)
				.setIsThrowExceptionIfIdentifierIsBlank(Boolean.TRUE)
				.setIsThrowExceptionIfResultIsBlank(Boolean.TRUE)
				);
		accept(request);
	}
	
	@Override @Transactional
	public void reject(Request request) {
		if(request == null)
			throw new RuntimeException("La demande est obligatoire");
		if(StringHelper.isBlank(request.getRejectionReason()))
			throw new RuntimeException("Le motif de rejet est obligatoire");
		request.setStatus(EntityFinder.getInstance().find(RequestStatus.class, RequestStatus.CODE_REJECTED));
		request.setProcessingDate(LocalDateTime.now());
		EntitySaver.getInstance().save(Request.class, new Arguments<Request>()
				.setPersistenceArguments(new org.cyk.utility.__kernel__.persistence.EntitySaver.Arguments<Request>().setUpdatables(List.of(request))));
	}
	
	@Override @Transactional
	public void rejectByIdentifier(String identifier, String rejectionReason) {
		Request request = EntityFinder.getInstance().find(Request.class, new QueryExecutorArguments().addSystemIdentifiers(identifier)
				.setIsThrowExceptionIfIdentifierIsBlank(Boolean.TRUE)
				.setIsThrowExceptionIfResultIsBlank(Boolean.TRUE)
				);
		request.setRejectionReason(rejectionReason);
		reject(request);
	}
}