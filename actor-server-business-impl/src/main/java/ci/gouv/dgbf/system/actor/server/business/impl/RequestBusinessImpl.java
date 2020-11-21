package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collection;

import javax.enterprise.context.ApplicationScoped;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.properties.Properties;
import org.cyk.utility.__kernel__.throwable.ThrowableHelper;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;
import org.cyk.utility.server.business.BusinessFunctionCreator;
import org.cyk.utility.server.business.BusinessServiceProvider;

import ci.gouv.dgbf.system.actor.server.business.api.RequestBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.RequestPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.IdentificationFormQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Request;

@ApplicationScoped
public class RequestBusinessImpl extends AbstractBusinessEntityImpl<Request, RequestPersistence> implements RequestBusiness,Serializable {
	private static final long serialVersionUID = 1L;
	
	@Override
	public BusinessServiceProvider<Request> save(Request request, Properties properties) {
		ThrowableHelper.throwIllegalArgumentExceptionIfNull("La demande est obligatoire", request);
		ThrowableHelper.throwIllegalArgumentExceptionIfNull("Le type de demande est obligatoire", request.getType());
		ThrowableHelper.throwIllegalArgumentExceptionIfNull("Le formulaire de demande est obligatoire", request.getType().getForm());
		ThrowableHelper.throwIllegalArgumentExceptionIfNull("L'acteur qui demande est obligatoire", request.getActor());
		IdentificationFormQuerier.AbstractImpl.setFields(request.getType().getForm(), null);
		if(CollectionHelper.isNotEmpty(request.getType().getForm().getAttributs())) {
			Collection<String> messages = new ArrayList<>();
			request.getType().getForm().getAttributs().forEach(attribut -> {
				
			});
		}
		return super.save(request, properties);
	}
	
	@Override
	public BusinessServiceProvider<Request> save(Request request) {
		ThrowableHelper.throwIllegalArgumentException("La demande est obligatoire", request);
		ThrowableHelper.throwIllegalArgumentException("Le type de demande est obligatoire", request.getType());
		ThrowableHelper.throwIllegalArgumentException("L'acteur qui demande est obligatoire", request.getActor());
		return this;
	}
	
	@Override
	protected void __listenExecuteCreateBefore__(Request request, Properties properties,BusinessFunctionCreator function) {
		super.__listenExecuteCreateBefore__(request, properties, function);
		request.setCreationDate(LocalDateTime.now());
	}
	
}
