package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.ModuleBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.ModulePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Module;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class ModuleBusinessImpl extends AbstractBusinessEntityImpl<Module, ModulePersistence> implements ModuleBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
