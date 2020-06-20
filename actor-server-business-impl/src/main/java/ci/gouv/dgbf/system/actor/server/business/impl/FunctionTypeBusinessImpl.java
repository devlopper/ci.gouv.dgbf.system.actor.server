package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.FunctionTypeBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.FunctionTypePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.FunctionType;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class FunctionTypeBusinessImpl extends AbstractBusinessEntityImpl<FunctionType, FunctionTypePersistence> implements FunctionTypeBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
