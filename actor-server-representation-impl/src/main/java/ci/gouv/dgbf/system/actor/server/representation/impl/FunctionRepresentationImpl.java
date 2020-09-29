package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

import ci.gouv.dgbf.system.actor.server.representation.api.FunctionRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.FunctionDto;

@ApplicationScoped
public class FunctionRepresentationImpl extends AbstractRepresentationEntityImpl<FunctionDto> implements FunctionRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}
