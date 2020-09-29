package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.ModuleRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.ModuleDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class ModuleRepresentationImpl extends AbstractRepresentationEntityImpl<ModuleDto> implements ModuleRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}
