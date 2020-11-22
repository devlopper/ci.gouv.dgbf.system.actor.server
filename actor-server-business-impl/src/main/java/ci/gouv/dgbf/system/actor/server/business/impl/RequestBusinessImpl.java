package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.Map;

import javax.enterprise.context.ApplicationScoped;

import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.__kernel__.map.MapHelper;
import org.cyk.utility.__kernel__.properties.Properties;
import org.cyk.utility.__kernel__.throwable.ThrowableHelper;
import org.cyk.utility.__kernel__.throwable.ThrowablesMessages;
import org.cyk.utility.__kernel__.value.ValueHelper;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;
import org.cyk.utility.server.business.BusinessFunctionCreator;
import org.cyk.utility.server.business.BusinessServiceProvider;

import ci.gouv.dgbf.system.actor.server.business.api.RequestBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.RequestPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.IdentificationFormQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.IdentificationAttribut;
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
		Map<String,IdentificationAttribut> attributs = Request.computeFieldsNames(request.getType().getForm());
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
		return super.save(request, properties);
	}
	
	@Override
	protected void __listenExecuteCreateBefore__(Request request, Properties properties,BusinessFunctionCreator function) {
		super.__listenExecuteCreateBefore__(request, properties, function);
		request.setCreationDate(LocalDateTime.now());
	}
	
}
