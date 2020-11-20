package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;
import java.time.LocalDateTime;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.RequestBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.RequestPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Request;

import org.cyk.utility.__kernel__.properties.Properties;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;
import org.cyk.utility.server.business.BusinessFunctionCreator;

@ApplicationScoped
public class RequestBusinessImpl extends AbstractBusinessEntityImpl<Request, RequestPersistence> implements RequestBusiness,Serializable {
	private static final long serialVersionUID = 1L;
	
	@Override
	protected void __listenExecuteCreateBefore__(Request request, Properties properties,BusinessFunctionCreator function) {
		super.__listenExecuteCreateBefore__(request, properties, function);
		request.setCreationDate(LocalDateTime.now());
	}
	
}
