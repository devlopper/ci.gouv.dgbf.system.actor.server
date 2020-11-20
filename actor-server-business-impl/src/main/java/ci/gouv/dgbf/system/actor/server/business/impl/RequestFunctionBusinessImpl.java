package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.RequestFunctionBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.RequestFunctionPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestFunction;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class RequestFunctionBusinessImpl extends AbstractBusinessEntityImpl<RequestFunction, RequestFunctionPersistence> implements RequestFunctionBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
